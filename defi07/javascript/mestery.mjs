#! Je sais, c'est hideux.
// Je sais, c'est hideux.
/* Je sais, c'est hideux. */

import { createInterface } from 'readline';

const GOLDEN_NUMBER = 0.5 + Math.sqrt(5) / 2;

const OPERATORS = {
  '+': [(rhs, lhs) => lhs + rhs, 0],
  '-': [(rhs, lhs) => lhs - rhs, 0],
  '*': [(rhs, lhs) => lhs * rhs, 1],
  '/': [(rhs, lhs) => lhs / rhs, 1],
  '%': [(rhs, lhs) => lhs % rhs, 1],
  '^': [(rhs, lhs) => lhs ** rhs, 2],
  '!': [(lhs) => {
    if (!Number.isInteger(lhs) || lhs < 0) {
      throw new Error('Factorial called on float or negative number.');
    }
    let result = 1;
    for (let i = 2; i < lhs; i++) {
      result *= i;
    }
    return result * lhs;
  }],
  fibonacci: [(n) => {
    if (!Number.isInteger(n) || n < 0) {
      throw new Error('Fibonacci called on float or negative number.');
    }
    return Math.trunc((1 / Math.sqrt(5)) * (GOLDEN_NUMBER ** n - 1 / GOLDEN_NUMBER ** n));
  }],
};

for (const k of Object.getOwnPropertyNames(Math)) {
  if (k !== 'sign' && typeof Math[k] === 'function' && Math[k].length === 1) {
    OPERATORS[k] = [Math[k]];
  }
}

const NUMBERS = {
  π: Math.PI,
  pi: Math.PI,
  e: Math.E,
};

function parseNumber(input) {
  const n = Number(input);
  if (n.toString() !== input) {
    throw new Error('Invalid expression.');
  }
  return n;
}

function evalPostfix(tokens) {
  const stack = [];
  for (const token of tokens) {
    if (token in OPERATORS) {
      if (stack.length < OPERATORS[token].length) {
        throw new Error('Invalid expression.');
      }
      stack.push(OPERATORS[token][0](...Array.from({ length: OPERATORS[token].length }, () => stack.pop())));
      continue;
    }

    stack.push(typeof token === 'number' ? token : (NUMBERS[token] ?? parseNumber(token)));
  }
  if (stack.length !== 1) {
    throw new Error('Invalid expression.');
  }
  return stack[0];
}

function* shuntingYard(tokens) {
  const opstack = [];

  for (let token of tokens) {
    if (token in OPERATORS) {
      // function
      if (token.length > 1 || token === '!') {
        opstack.push(token);
        continue;
      }

      // operator
      while (
        opstack.length &&
        opstack[opstack.length - 1] !== '(' &&
        (OPERATORS[opstack[opstack.length - 1]][1] > OPERATORS[token][1] ||
          (token !== '^' && OPERATORS[opstack[opstack.length - 1]][1] === OPERATORS[token][1]))
      ) {
        yield opstack.pop();
      }
      opstack.push(token);
      continue;
    }

    // left parenthesis
    if (token === '(') {
      opstack.push('(');
      continue;
    }

    // right parenthesis
    if (token === ')') {
      let op;
      while (opstack.length && (op = opstack.pop()) !== '(') {
        yield op;
      }
      if (opstack.length && (opstack[opstack.length - 1].length > 1 || opstack[opstack.length - 1] === '!')) {
        yield opstack.pop();
      }
      continue;
    }

    yield token;
  }
  yield* opstack.reverse();
}

const OPERATORS_SUBSET = ['+', '-', '*', '/', '^', '%'];

function tokenizeInfix(expression) {
  const tokens = [];

  for (let i = 0; i < expression.length; i++) {
    const char = expression[i];
    if (char === ' ') {
      continue;
    }

    if ((char === '+' || char === '-') && (tokens.length === 0 || OPERATORS_SUBSET.concat('(').includes(tokens[tokens.length - 1])) && char[i + 1] !== ' ') {
      if (char === '-') tokens.push(-1, '*');
    } else if (char >= '0' && char <= '9') {
      let number = '';
      while (expression[i] >= '0' && expression[i] <= '9' || expression[i] === '.') {
        if (expression[i] === '.' && number.includes('.')) {
          throw new Error('Invalid expression.');
        }
        number += expression[i++];
      }
      i--;
      tokens.push(parseNumber(number));
    } else if ([...OPERATORS_SUBSET, '!'].includes(char)) {
      if (char === '!') {
        if (![')', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'e', 'i' /* pi */, 'π'].includes(expression[i - 1])) {
          throw new Error('Invalid expression.');
        }
      }
      tokens.push(char);
    } else if (['(', ')', 'e', 'π'].includes(char)) {
      tokens.push(char);
    } else if (Object.keys(OPERATORS).some((value) => value.length > 1 && value[0] === char)) {
      let func = char;
      while (expression[i + 1].toLowerCase() >= 'a' && expression[i + 1].toLowerCase() <= 'z' || expression[i + 1] >= '0' && expression[i + 1] <= '9') {
        func += expression[++i];
      }
      if (!(func in OPERATORS)) {
        throw new Error('Invalid expression.');
      }
      tokens.push(func);
    } else {
      throw new Error('Invalid expression.');
    }
  }

  return tokens;
}

function tokenizePostfix(expression) {
  return expression.toLowerCase().split(/\s+/);
}

function evalExpression(expression) {
  try {
    return evalPostfix(shuntingYard(tokenizeInfix(expression)));
  } catch (error) {
    try {
      return evalPostfix(tokenizePostfix(expression));
    } catch (error2) {
      throw new Error((error.message.length > error2.message.length ? error : error2).message);
    }
  }
}

function ansi(string, effect) {
  return `\u001b[${effect}m${string}\u001b[0m`;
}

function logResult(result) {
  console.log(ansi(result, '33'));
}

const rl = createInterface(process.stdin, process.stdout);
const PREFIX = `${ansi('>', '36;1')} `;

rl.on('line', (line) => {
  line = line.trim();
  if (!line || line === 'h') {
    console.error(`${ansi('Type an infix or postfix expression, or a command:', '92')}
 - "${ansi('e <expr>', '1')}" evaluate an infix or postfix expression
 - "${ansi('ei <infix expr>', '1')}" evaluate an infix expression
 - "${ansi('ep <postfix expr>', '1')}" evaluate an postfix expression
 - "${ansi('i2p <infix expr>', '1')}" transform an infix to postfix expression
 - "${ansi('h', '1')}" show this`);
  } else {
    try {
      if (line.startsWith('e ')) {
        logResult(evalExpression(line.slice(2)));
      } else if (line.startsWith('ei ')) {
        logResult(evalPostfix(shuntingYard(line.slice(3))));
      } else if (line.startsWith('ep ')) {
        logResult(evalPostfix(tokenizePostfix(line.slice(3))));
      } else if (line.startsWith('i2p ')) {
        logResult([...shuntingYard(line.slice(4))].join(' '));
      } else {
        logResult(evalExpression(line));
      }
    } catch (error) {
      console.error(ansi(error.message, '91'));
    }
  }

  rl.setPrompt(PREFIX);
  rl.prompt();
});

rl.setPrompt(PREFIX);
rl.prompt();
