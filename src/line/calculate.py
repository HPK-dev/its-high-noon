import re

def calculate(expression: str):
    def parse_expression(expr):
        def apply_operator(operators, values):
            operator = operators.pop()
            b = values.pop()
            a = values.pop()
            if operator == '+':
                values.append(a + b)
            elif operator == '-':
                values.append(a - b)
            elif operator == '*':
                values.append(a * b)
            elif operator == '/':
                if b == 0:
                    raise ZeroDivisionError("Division by zero")
                values.append(a / b)
            elif operator == '**':
                values.append(a ** b)
            elif operator == '%':
                values.append(a % b)

        def precedence(op):
            if op in ('+', '-'):
                return 1
            elif op in ('*', '/', '%'):
                return 2
            elif op == '**':
                return 3
            return 0

        tokens = re.findall(r'\d+\.?\d*|[+\-*/%^()]', expr)
        values = []
        operators = []

        i = 0
        while i < len(tokens):
            token = tokens[i]
            if re.match(r'\d+\.?\d*', token):
                values.append(float(token))
            elif token == '(':
                operators.append(token)
            elif token == ')':
                while operators and operators[-1] != '(':
                    apply_operator(operators, values)
                operators.pop()
            else:
                while (operators and operators[-1] != '(' and
                       precedence(operators[-1]) >= precedence(token)):
                    apply_operator(operators, values)
                operators.append(token)
            i += 1

        while operators:
            apply_operator(operators, values)

        return values[0]

    try:
        expression = expression.replace('^', '**')
        return parse_expression(expression)
    except ZeroDivisionError as e:
        return f"Error: {str(e)}"
    except Exception as e:
        return "Error: Invalid expression"