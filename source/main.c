#include <gccore.h>
#include <wiiuse/wpad.h>
#include <wiikeyboard/keyboard.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#define MAX_INPUT 256
#define MAX_TOKENS 100
#define MAX_STACK 100

float calculate(float a, float b, char op);
long long factorial(int n);
int gcd(int a, int b);
int lcm(int a, int b);

typedef struct {
    float value;
    char operator;
    bool isNumber;
} Token;

static void *xfb = NULL;
static GXRModeObj *rmode = NULL;
bool quitapp = false;
char input_buffer[MAX_INPUT];
int buffer_pos = 0;

int getPrecedence(char op) {
    switch(op) {
        case '!': // Factorial (unary, high precedence)
        case 's': // sin
        case 'c': // cos
        case 't': // tan
        case 'n': // ln
        case 'l': // log10
        case 'r': // sqrt
        case 'a': // abs
        case 'h': // sinh
        case 'f': // floor
        case 'e': // ceil
            return 4;
        case '^': return 3;
        case '*': // multiply
        case '/': // divide
        case '%': return 2;
        case '+': // add
        case '-': return 1;
        case 'g': // gcd
        case 'm': // lcm
            return 0; // Lower precedence for these
        default: return -1;
    }
}

bool isOperator(char c) {
    return c == '+' || c == '-' || c == '*' || c == '/' ||
           c == '^' || c == '%' || c == 's' || c == 'c' ||
           c == 't' || c == 'n' || c == 'l' || c == '!' ||
           c == 'a' || c == 'h' || c == 'g' || c == 'm' ||
           c == 'f' || c == 'e' || c == 'r';
}

int tokenize(const char* input, Token* tokens) {
    int count = 0;
    char temp[MAX_INPUT];
    strcpy(temp, input);
    char* token = strtok(temp, " ");

    while (token != NULL && count < MAX_TOKENS) {
        if (strlen(token) == 1 && isOperator(token[0])) {
            tokens[count].isNumber = false;
            tokens[count].operator = token[0];
        } else {
            tokens[count].isNumber = true;
            tokens[count].value = atof(token);
        }
        count++;
        token = strtok(NULL, " ");
    }

    return count;
}

int infixToPostfix(Token* infix, int infixCount, Token* postfix) {
    Token stack[MAX_STACK];
    int stackTop = 0;
    int postfixCount = 0;

    for (int i = 0; i < infixCount; i++) {
        if (infix[i].isNumber) {
            postfix[postfixCount++] = infix[i];
        } else {
            while (stackTop > 0 && getPrecedence(stack[stackTop - 1].operator) >= getPrecedence(infix[i].operator)) {
                postfix[postfixCount++] = stack[--stackTop];
            }
            stack[stackTop++] = infix[i];
        }
    }

    while (stackTop > 0) {
        postfix[postfixCount++] = stack[--stackTop];
    }

    return postfixCount;
}

float evaluatePostfix(Token* postfix, int count) {
    float stack[MAX_STACK];
    int stackTop = 0;

    for (int i = 0; i < count; i++) {
        if (postfix[i].isNumber) {
            stack[stackTop++] = postfix[i].value;
        } else {
            char op = postfix[i].operator;
            if (op == '!' || op == 's' || op == 'c' || op == 't' || op == 'n' || op == 'l' || op == 'r' || op == 'a' || op == 'h' || op == 'f' || op == 'e') {
                if (stackTop < 1) return 0;
                float a = stack[--stackTop];
                stack[stackTop++] = calculate(a, 0, op);
            } else {
                if (stackTop < 2) return 0;
                float b = stack[--stackTop];
                float a = stack[--stackTop];
                stack[stackTop++] = calculate(a, b, op);
            }
        }
    }

    if (stackTop == 1) {
        return stack[0];
    } else {
        return 0;
    }
}

long long factorial(int n) {
    if (n < 0) return 0;
    if (n <= 1) return 1;
    long long res = 1;
    for (int i = 2; i <= n; i++) {
        res *= i;
    }
    return res;
}

int gcd(int a, int b) {
    a = abs(a);
    b = abs(b);
    while (b) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

int lcm(int a, int b) {
    if (a == 0 || b == 0) return 0;
    return abs(a * b) / gcd(a, b);
}

float calculate(float a, float b, char op) {
    switch(op) {
        case '+': return a + b;
        case '-': return a - b;
        case '*': return a * b;
        case '/':
            if (b == 0) {
                printf("\x1b[31mError: Division by zero!\x1b[0m\n");
                return 0;
            }
            return a / b;
        case '^': return powf(a, b);
        case '%':
            if (b == 0) {
                printf("\x1b[31mError: Modulo by zero!\x1b[0m\n");
                return 0;
            }
            return fmodf(a, b);
        case 'r':
            if (a < 0) {
                printf("\x1b[31mError: Square root of negative number!\x1b[0m\n");
                return 0;
            }
            return sqrtf(a);
        case 's': return sinf(a * M_PI / 180.0f);  // sine (degrees)
        case 'c': return cosf(a * M_PI / 180.0f);  // cosine (degrees)
        case 't': return tanf(a * M_PI / 180.0f);  // tangent (degrees)
        case 'n':
            if (a <= 0) {
                printf("\x1b[31mError: Natural logarithm of non-positive number!\x1b[0m\n");
                return 0;
            }
            return logf(a);                   // natural log
        case 'l':
            if (a <= 0) {
                printf("\x1b[31mError: Log base 10 of non-positive number!\x1b[0m\n");
                return 0;
            }
            return log10f(a);                // log base 10
        case '!': return (float)factorial((int)a);  // factorial
        case 'a': return fabsf(a);                 // absolute
        case 'h': return sinhf(a);                 // hyperbolic sine
        case 'g': return (float)gcd((int)a, (int)b); // GCD
        case 'm': return (float)lcm((int)a, (int)b); // LCM
        case 'f': return floorf(a);                // floor
        case 'e': return ceilf(a);                 // ceil
        default: return 0;
    }
}

void execute_command(const char *cmd) {
    if (strcmp(cmd, "help") == 0) {
        printf("\x1b[36m=== Wii Calculator Help ===\x1b[0m\n");
        printf("\x1b[33mSystem Commands:\x1b[0m\n");
        printf("  help  - Show this help\n");
        printf("  clear - Clear screen\n");
        printf("  quit  - Exit program\n\n");
        printf("\x1b[33mCalculator Operations:\x1b[0m\n");
        printf("Format: number operator number (for binary operations)\n");
        printf("        operator number (for unary operations)\n");
        printf("Supported operators:\n");
        printf("  +  Addition       (ex: 5 + 3)\n");
        printf("  -  Subtraction    (ex: 10 - 4)\n");
        printf("  *  Multiplication (ex: 6 * 2)\n");
        printf("  /  Division       (ex: 15 / 3)\n");
        printf("  ^  Power         (ex: 2 ^ 3)\n");
        printf("  %%  Modulo        (ex: 7 %% 4)\n");
        printf("  r  Square root   (ex: r 16)\n");
        printf("  s  Sine          (ex: s 90)\n");
        printf("  c  Cosine        (ex: c 45)\n");
        printf("  t  Tangent       (ex: t 45)\n");
        printf("  n  Natural log   (ex: n 2.7)\n");
        printf("  l  Log base 10   (ex: l 100)\n");
        printf("  !  Factorial     (ex: ! 5)\n");
        printf("  a  Absolute      (ex: a -5)\n");
        printf("  g  GCD           (ex: 24 g 18)\n");
        printf("  m  LCM           (ex: 6 m 8)\n");
        printf("  f  Floor         (ex: f 3.7)\n");
        printf("  e  Ceil          (ex: e 3.2)\n");
    }

    else if (strcmp(cmd, "clear") == 0) {
        printf("\x1b[2J\x1b[H");  // ANSI escape sequence to clear screen and move cursor to home
        printf("Wii REPL v1.0\n");
        printf("Type 'help' for commands\n");
    }

    else if (strcmp(cmd, "quit") == 0) {
        quitapp = true;
    }

    else {
        Token infix[MAX_TOKENS];
        Token postfix[MAX_TOKENS];
        int tokenCount = tokenize(cmd, infix);

        if (tokenCount > 0) {
            int postfixCount = infixToPostfix(infix, tokenCount, postfix);
            float result = evaluatePostfix(postfix, postfixCount);
            printf("\x1b[32m%s = %.2f\x1b[0m\n", cmd, result);
        }
        else if (strlen(cmd) > 0) {
            printf("\x1b[31mInvalid command or expression\x1b[0m\n");
        }
    }
}

void keyPress_cb(char sym) {
    if (sym == 0x1b) quitapp = true;
}

int main(int argc, char **argv) {
    VIDEO_Init();
    WPAD_Init();
    rmode = VIDEO_GetPreferredMode(NULL);
    xfb = MEM_K0_TO_K1(SYS_AllocateFramebuffer(rmode));
    console_init(xfb,20,20,rmode->fbWidth,rmode->xfbHeight,rmode->fbWidth*VI_DISPLAY_PIX_SZ);
    VIDEO_Configure(rmode);
    VIDEO_SetNextFramebuffer(xfb);
    VIDEO_SetBlack(false);
    VIDEO_Flush();
    VIDEO_WaitVSync();
    if(rmode->viTVMode&VI_NON_INTERLACE) VIDEO_WaitVSync();

    printf("\x1b[2;0HCalculator REPL v1.0\n");
    printf("Type 'help' for commands. Type 'quit' or hold ESC to exit.\n");
    printf("> ");

    fflush(stdout);

    if (KEYBOARD_Init(keyPress_cb) == 0) {
        printf("Keyboard initialized\n");
        fflush(stdout);
    } else {
        printf("Keyboard initialization failed!\n");
        fflush(stdout);
        return 1;
    }

    while(!quitapp) {
        WPAD_ScanPads();

        if (WPAD_ButtonsDown(0) & WPAD_BUTTON_HOME) {
            quitapp = true;
            break;
        }

        int key = getchar();
        if(key != EOF) {
            if (key == '\n' || key == '\r') {
                putchar('\n');
                input_buffer[buffer_pos] = '\0';
                execute_command(input_buffer);
                buffer_pos = 0;
                printf("> ");
                fflush(stdout);
            }
            else if ((key == '\b' || key == 127) && buffer_pos > 0) {
                buffer_pos--;
                printf("\b \b");
                fflush(stdout);
            }
            else if (key >= 32 && key < 127 && buffer_pos < MAX_INPUT - 1) {
                input_buffer[buffer_pos++] = key;
                putchar(key);
                fflush(stdout);
            }
        }

        VIDEO_WaitVSync();
    }

    KEYBOARD_Deinit();
    return 0;
}