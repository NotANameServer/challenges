enum Operator {
    PARENTHESIS_OPEN = '(',
    PARENTHESIS_CLOSE = ')',
    PLUS = '+',
    MINUS = '-',
    MULTIPLY = '*',
    DIVIDE = '/',
    POWER = '^',
    FACTORIAL = '!'
}

// Operator priority
enum Priority {
    LOW = 1, // + and -
    MEDIUM = 2, // * and /
    HIGH = 3 // ^ and !
}

// Factorial calculator
class Factorial {
    public static compute(value: number): number {
        return value > 1 ? value * Factorial.compute(value - 1) : 1;
    }
}

// Operation calculator
class Operation {
    private _operator: Operator;
    private _priority: Priority;
    private _leftOperand: number | null;
    private _rightOperand: number | null;

    private _verbose: boolean;
    private _listener(...args: any[]): void {
        console.log(...args);
    }
    private _callListener(...args: any[]): void {
        if (this._verbose) {
            this._listener(...args);
        }
    }


    public get priority(): Priority {
        return this._priority;
    }

    public get nbOperands(): number {
        return this._operator !== Operator.FACTORIAL ? 2 : 1;
    }

    public set leftOperand(value: number) {
        this._leftOperand = value;
    }

    public set rightOperand(value: number) {
        this._rightOperand = value;
    }

    public constructor(operator: Operator, verbose: boolean = false) {
        this._operator = operator;
        this._leftOperand = null;
        this._rightOperand = null;
        switch (this._operator) {
            case Operator.PLUS:
            case Operator.MINUS:
                this._priority = Priority.LOW;
                break;
            case Operator.MULTIPLY:
            case Operator.DIVIDE:
                this._priority = Priority.MEDIUM;
                break;
            default:
                this._priority = Priority.HIGH;
                break;
        }
        this._verbose = verbose;
    }

    public compute(): number {
        switch (this._operator) {
            case Operator.PLUS:
                this._callListener(`${this._leftOperand} ${this._operator} ${this._rightOperand} = ${this._leftOperand + this._rightOperand}`);
                return this._leftOperand + this._rightOperand;
            case Operator.MINUS:
                this._callListener(`${this._leftOperand} ${this._operator} ${this._rightOperand} = ${this._leftOperand - this._rightOperand}`);
                return this._leftOperand - this._rightOperand;
            case Operator.MULTIPLY:
                this._callListener(`${this._leftOperand} ${this._operator} ${this._rightOperand} = ${this._leftOperand * this._rightOperand}`);
                return this._leftOperand * this._rightOperand;
            case Operator.DIVIDE:
                this._callListener(`${this._leftOperand} ${this._operator} ${this._rightOperand} = ${this._leftOperand / this._rightOperand}`);
                return this._leftOperand / this._rightOperand;
            case Operator.POWER:
                this._callListener(`${this._leftOperand} ${this._operator} ${this._rightOperand} = ${Math.pow(this._leftOperand, this._rightOperand)}`);
                return Math.pow(this._leftOperand, this._rightOperand);
            case Operator.FACTORIAL:
                this._callListener(`${this._leftOperand}${this._operator} = ${Factorial.compute(this._leftOperand)}`);
                return Factorial.compute(this._leftOperand);
        }
    }
}

// Custom Stack data structure
class Stack<T> {
    private _stack: T[];

    public get length(): number {
        return this._stack.length;
    }

    public constructor(values: T[] = []) {
        this._stack = [...values];
    }

    public push(value: T) {
        this._stack.unshift(value);
    }

    public pop(index: number = 0): T {
        const value: T = this._stack[index];
        this._stack.splice(index, 1);
        return value;
    }

    public reverse(): void {
        this._stack.reverse();
    }

    public forEach(callback: (value: T, index: number) => void) {
        this._stack.forEach(callback);
    }

    public get(index: number): T {
        return this._stack[index];
    }

    public set(index: number, value: T): void {
        this._stack[index] = value;
    }
}

// Mathematical expression parser and solver
class MathExpression {
    private _input: string;
    private _expression: string;
    private _operators: Operator[];
    private _numbers: number[];
    private _parsedExpression: (Operator | number)[];

    private _verbose: boolean;
    private _listener(...args: any[]): void {
        console.log(...args);
    }
    private _callListener(...args: any[]): void {
        if (this._verbose) {
            this._listener(...args);
        }
    }

    constructor(mathExpression: string, verbose: boolean = false) {
        this._input = mathExpression;
        this._verbose = verbose;
    }

    private _trim(): void {
        this._expression = this._input.replaceAll(' ', '');
    }

    private _parseOperators(): void {
        this._operators = this._expression
            .split(/\d+\.\d+|\d+/g)
            .join('')
            .split('') as Operator[];
    }

    private _parseOperands(): void {
        this._numbers = this._expression
            .split(/\+|-|\*|\/|\(|\)|\^|!/g)
            .filter((element: string) => element !== '')
            .map((numberInput: string) => Number(numberInput));
    }

    private _buildParsedExpression(): void {
        this._parsedExpression = [];
        const numbers: Stack<number> = new Stack<number>(this._numbers);
        this._operators.forEach((currentOperator: Operator, index: number) => {
            const previousOperator: Operator | null = this._operators[index - 1] ?? null;
            const nextOperator: Operator | null = this._operators[index + 1] ?? null;
            switch (currentOperator) {
                case Operator.PARENTHESIS_OPEN:
                    this._parsedExpression.push(currentOperator);
                    if (nextOperator === Operator.PARENTHESIS_CLOSE) {
                        this._parsedExpression.push(numbers.pop());
                    }
                    break;
                case Operator.PARENTHESIS_CLOSE:
                    this._parsedExpression.push(currentOperator);
                    break;
                case Operator.FACTORIAL:
                    if (previousOperator === Operator.PARENTHESIS_CLOSE || !Number.isNaN(Number(this._parsedExpression[this._parsedExpression.length - 1]))) {
                        this._parsedExpression.push(currentOperator);
                    }
                    else {
                        this._parsedExpression.push(numbers.pop());
                        this._parsedExpression.push(currentOperator);
                    }
                    break;
                case Operator.PLUS:
                case Operator.MINUS:
                case Operator.MULTIPLY:
                case Operator.DIVIDE:
                case Operator.POWER:
                    if (previousOperator === Operator.PARENTHESIS_CLOSE || previousOperator === Operator.FACTORIAL || !Number.isNaN(Number(this._parsedExpression[this._parsedExpression.length - 1]))) {
                        this._parsedExpression.push(currentOperator);
                    }
                    else {
                        this._parsedExpression.push(numbers.pop());
                        this._parsedExpression.push(currentOperator);
                    }
                    if (nextOperator !== Operator.PARENTHESIS_OPEN) {
                        this._parsedExpression.push(numbers.pop());
                    }
                    break;
            }
        });
        if (numbers.length === 1) { // happens when input is only a number
            this._parsedExpression.push(numbers.pop());
        }
    }

    private _parse(): void {
        this._parseOperators();
        this._parseOperands();
        this._buildParsedExpression();
    }

    private _prepare(): void {
        this._trim();
        this._parse();
    }

    private _establishOperatorTreatmentOrder(operators: Stack<Operator>): Stack<number> {
        const treatmentOrder: Stack<number> = new Stack<number>();
        const lowTreatmentOrder: Stack<number> = new Stack<number>();
        operators.forEach((operator: Operator, index: number) => {
            const operation: Operation = new Operation(operator, this._verbose);
            if (operation.priority === Priority.LOW) {
                lowTreatmentOrder.push(index);
            }
        });
        while (lowTreatmentOrder.length > 0) {
            treatmentOrder.push(lowTreatmentOrder.pop());
        }
        const mediumTreatmentOrder: Stack<number> = new Stack<number>();
        operators.forEach((operator: Operator, index: number) => {
            const operation: Operation = new Operation(operator, this._verbose);
            if (operation.priority === Priority.MEDIUM) {
                mediumTreatmentOrder.push(index);
            }
        });
        while (mediumTreatmentOrder.length > 0) {
            treatmentOrder.push(mediumTreatmentOrder.pop());
        }
        const highTreatmentOrder: Stack<number> = new Stack<number>();
        operators.forEach((operator: Operator, index: number) => {
            const operation: Operation = new Operation(operator, this._verbose);
            if (operation.priority === Priority.HIGH) {
                highTreatmentOrder.push(index);
            }
        });
        while (highTreatmentOrder.length > 0) {
            treatmentOrder.push(highTreatmentOrder.pop());
        }
        return treatmentOrder;
    }

    private _buildParenthesisExpressionParts(parsedExpression: Stack<Operator | number>): { operands: Stack<number>, operators: Stack<Operator> } {
        const operands: Stack<number> = new Stack<number>();
        const operators: Stack<Operator> = new Stack<Operator>();
        let operatorOrNumber: Operator | number = parsedExpression.pop();
        while (operatorOrNumber !== Operator.PARENTHESIS_OPEN) {
            if (!Number.isNaN(Number(operatorOrNumber))) {
                const number: number = operatorOrNumber as number;
                operands.push(number);
            }
            else {
                const operator: Operator = operatorOrNumber as Operator;
                operators.push(operator);
            }
            operatorOrNumber = parsedExpression.pop();
        }
        return {
            operands,
            operators
        };
    }

    private _buildExpressionParts(parsedExpression: Stack<Operator | number>): { operands: Stack<number>, operators: Stack<Operator> } {
        const operands: Stack<number> = new Stack<number>();
        const operators: Stack<Operator> = new Stack<Operator>();
        let operatorOrNumber: Operator | number = parsedExpression.pop();
        while (operatorOrNumber !== undefined) {
            if (!Number.isNaN(Number(operatorOrNumber))) {
                const number: number = operatorOrNumber as number;
                operands.push(number);
            }
            else {
                const operator: Operator = operatorOrNumber as Operator;
                operators.push(operator);
            }
            operatorOrNumber = parsedExpression.pop();
        }
        return {
            operands,
            operators
        };
    }

    private _solveExpression(operands: Stack<number>, operators: Stack<Operator>): number {
        if (operators.length === 0) {
            return operands.pop();
        }
        const treatmentOrder: Stack<number> = this._establishOperatorTreatmentOrder(operators);
        do {
            const currentOperatorIndex: number = treatmentOrder.pop();
            treatmentOrder.forEach((operatorIndex: number, index: number) => {
                if (operatorIndex > currentOperatorIndex) {
                    treatmentOrder.set(index, operatorIndex - 1);
                }
            });
            const operation: Operation = new Operation(operators.pop(currentOperatorIndex), this._verbose);
            operation.leftOperand = operands.get(currentOperatorIndex);
            if (operation.nbOperands === 2) {
                operation.rightOperand = operands.pop(currentOperatorIndex + 1);
            }
            operands.set(currentOperatorIndex, operation.compute());
        } while (treatmentOrder.length > 0);
        return operands.pop();
    }

    private _solve(): number {
        const parsedExpression: Stack<Operator | number> = new Stack<Operator | number>(this._parsedExpression);
        const result: Stack<Operator | number> = new Stack<Operator | number>();
        while (parsedExpression.length > 0) {
            const currentOperatorOrNumber: Operator | number = parsedExpression.pop();
            if (!Number.isNaN(Number(currentOperatorOrNumber))) {
                const currentNumber: number = currentOperatorOrNumber as number;
                result.push(currentNumber);
            }
            else {
                const currentOperator: Operator = currentOperatorOrNumber as Operator;
                if (currentOperator === Operator.PARENTHESIS_CLOSE) {
                    const parts = this._buildParenthesisExpressionParts(result);
                    result.push(this._solveExpression(parts.operands, parts.operators));
                }
                else {
                    result.push(currentOperator);
                }
            }
        }
        if (result.length > 1) { // usefull if whole expression is between parentheses
            const parts = this._buildExpressionParts(result);
            result.push(this._solveExpression(parts.operands, parts.operators));
        }
        return result.pop() as number;
    }

    public parse(): (Operator | number)[] {
        this._callListener(`Given input expression: ${this._input}`);
        this._prepare();
        this._callListener(`Parsed expression: `, this._parsedExpression);
        return this._parsedExpression;
    }

    public solve(): number {
        this._prepare();
        this._callListener("Step by step operations:");
        const result: number = this._solve();
        this._callListener("Solution found:");
        this._callListener(`${this._input} = ${result}`);
        return result;
    }
}

console.clear();
// const input: string = '(1 * ( 2 - 3 ) / 4) * 5 - 6 / 7';
// const input: string = '2 / 3 + 4 * 5 - 6 ^ 7 * 3! - 9';
const input: string = '(5 * (7 - 4) / 2) - 4.5 ^ 3 + 4!';
const mathExpression: MathExpression = new MathExpression(input, true);
mathExpression.parse();
mathExpression.solve();
  
