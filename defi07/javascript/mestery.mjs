import { createInterface } from 'readline';

const GOLDEN_NUMBER = 0.5 + Math.sqrt(5) / 2;

const OPERATORS = {
  '+': (roperand, loperand) => loperand + roperand,
  '-': (roperand, loperand) => loperand - roperand,
  '*': (roperand, loperand) => loperand * roperand,
  '/': (roperand, loperand) => loperand / roperand,
  '%': (roperand, loperand) => loperand % roperand,
  '^': (roperand, loperand) => loperand ** roperand,
  '!'(operand) {
    if (!Number.isInteger(operand) || operand < 0) {
      throw new Error('Factorial called on float or negative number.');
    }
    let result = 1;
    for (let i = 2; i < operand; i++) {
      result *= i;
    }
    return result * operand;
  },
  fibonacci(operand) {
    if (!Number.isInteger(operand) || operand < 0) {
      throw new Error('Fibonacci called on float or negative number.');
    }
    return Math.trunc((1 / Math.sqrt(5)) * (GOLDEN_NUMBER ** operand - 1 / GOLDEN_NUMBER ** operand));
  },
  // aliases
  '√': Math.sqrt,
  ln: Math.log,
};

for (const k of Object.getOwnPropertyNames(Math)) {
  if (k !== 'sign' && typeof Math[k] === 'function' && Math[k].length === 1) {
    OPERATORS[k] = Math[k];
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

function evalPostfix(postfix) {
  const stack = [];
  for (const token of postfix) {
    if (token in OPERATORS) {
      if (stack.length < OPERATORS[token].length) {
        throw new Error('Invalid expression.');
      }
      stack.push(OPERATORS[token](...Array.from({ length: OPERATORS[token].length }, () => stack.pop())));
      continue;
    }

    stack.push(NUMBERS[token] ?? parseNumber(token));
  }
  if (stack.length !== 1) {
    throw new Error('Invalid expression.');
  }
  return stack[0];
}

function precedence(operator) {
  if (operator === '^') {
    return 2;
  }
  return operator === '*' || operator === '/';
}

function* tokenizeInfix(expression) {
  const opstack = [];

  for (let token of expression.toLowerCase().split(/([\^!*%/()√]|\b\s*[-+])/)) {
    token = token.trim();

    if (!token) {
      continue;
    }

    if (token in OPERATORS) {
      // function
      if (token.length > 1 || token === '!' || token === '√') {
        opstack.push(token);
        continue;
      }

      // operator
      while (
        opstack.length &&
        opstack[opstack.length - 1] !== '(' &&
        (precedence(opstack[opstack.length - 1]) > precedence(token) ||
          (token !== '^' && precedence(opstack[opstack.length - 1]) === precedence(token)))
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

function tokenizePostfix(expression) {
  return expression.toLowerCase().split(/\s+/);
}

function evalExpression(expression) {
  try {
    return evalPostfix(tokenizeInfix(expression));
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
        logResult(evalPostfix(tokenizeInfix(line.slice(3))));
      } else if (line.startsWith('ep ')) {
        logResult(evalPostfix(tokenizePostfix(line.slice(3))));
      } else if (line.startsWith('i2p ')) {
        logResult([...tokenizeInfix(line.slice(4))].join(' '));
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
