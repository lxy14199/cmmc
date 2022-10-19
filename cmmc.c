#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>

#define int long long
int token; //读入的token
char *src, *old_src; //读入文件的源的指针
//数据端，代码端，栈端
int *text,
	*old_text,
	*stack;
char *data;
//
int *pc, *bp, *sp, ax, cycle;
//数据端，代码端，栈端的大小
int poolsize;
//读入在第几行
int line;

//指令集
enum {  LEA, IMM, JMP, CALL, JZ, JNZ, ENT, ADJ, LEV, LI, LC, SI, SC, PUSH,
		OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD,
		OPEN, READ, CLOS, PRTF, MALC, MSET, MCMP, EXIT};

//用来标识token, 便于词法分析
enum {
		Num = 128, Fun, Sys, Glo, Loc, Id, 
		Char, Else, Enum, If, Int, Return, Sizeof, While, 
		Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc,Dec, Brak
};

/*
struct identifier {
		int token;  //该标识符返回的标识
		int hash;   //计算的hash值，便于查找
		char *name; //存放名字
		int class;  //标识符对应类别，如数字、全局变量
		int type;   //标识符对应类型，int、char、ptr
		int value;  //存放对应的值，如果是函数则存放函数地址
		int Bclass; //用于区别局部变量和全局变量
		int Btype;
		int Bvalue;
};
*/
enum {Token, Hash, Name, Type, Class, value, BType, BClass, BValue, IdSize};  //等价于定义了identifier结构体
int token_value;
int *current_id,
	*symbols;


//用于词法分析指向下一个字符
void next() {
		char *last_pos;
		int hash;
		
		while(token = *src) {
				++ src;
				//词法分析
				if(token == '\n') {
						++ line;
				} else if (token == '#') {
						//cmmc无法解析宏定义
						while(*src != 0 && *src != '\n') {
								src ++;
						}
				} else if ((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z') || (token == '_')) {
						//分析定义的变量名
						last_pos = src - 1;
						hash = token;
						
						while((*src >= 'a' && *src <= 'z') || (*src >= 'A' && *src <= 'Z') || (*src >= '0' && *src <= '9') || (*src == '_')) {
								hash = hash * 147 + *src;
								src ++;
						}
						
						current_id = symbols;
						while(current_id[Token]) {
								if (current_id[Hash] == hash && !memcmp((char *)current_id[Name], last_pos, src - last_pos)) {
										token = current_id[Token];
										return ;
								}
								current_id = current_id + IdSize;
						}

						current_id[Name] = (int)last_pos;
						current_id[Hash] = hash;
						token = current_id[Token] = Id;
						return ;		
				} else if (token >= '0' && token <= '9') {
						//解析3种进制，16进制，10进制，8进制
						token_value = token - '0';
						if (token_value > '0') {
								while(*src >= '0' && *src <= '9') {
										token_value = token_value * 10 + *src++ - '0';
								}
						} else {
								if(*src == 'x' || *src == 'X') {
										token = *++src;
										while((token >= '0' && token <= '9') || (token >= 'a' && token <= 'f') || (token >= 'A' && token <= 'F')) {
												token_value = token_value * 16 + (token & 15) + (token >= 'A' ? 9 : 0);
												token = ++src;
										}
								} else {
										while(*src >= '0' && *src <= '7') {
												token_value = token_value * 8 + *src ++ - '0';
										}
								}
						}
						token = Num;
						return;
				} else if(token == '"' || token == '\'') {
						//解析字符串
						last_pos = data;
						while(*src != 0 && *src != token) {
								token_value = *src ++;
								if(token_value == '\\') {
										token_value = *src ++;
										if(token_value == 'n') {
												token_value = '\n';
										}		
								}
								if(token == '"') {
										*data = token_value;
								}
						}
						src ++;
						if(token == '"') {
								token_value = (int)last_pos;
						} else {
								token = Num;
						}

						return ;
				} else if(token == '/') {
						if (*src == '/') {
								//跳过注释
								while(*src != 0 && *src != '\n') {
										++ src;
								}
						} else {
								token = Div;
								return;
						}
				} else if(token == '=') {
						if (*src == '=') {
								src ++;
								token = Eq;
						} else {
								token = Assign;
						}
						return ;
				} else if(token == '+') {
						if (*src == '+') {
								src ++;
								token = Inc;
						} else {
								token = Add;
						}
						return ;
				} else if(token == '-') {
						if(*src == '-') {
								src ++;
								token = Dec;
						} else {
								token = Sub;
						}
						return ;
				} else if(token == '!') {
						if(*src == '=') {
								src ++;
								token = Ne;
						}
						return ;
				} else if(token == '<') {
						if(*src == '=') {
								src ++;
								token = Le;
						} else if(*src == '<') {
								src ++;
								token = Shl;
						} else {
								token = Lt;
						}
						return ;
				} else if(token == '>') {
						if(*src == '=') {
								src ++;
								token = Ge;
						} else if(*src == '>') {
								src ++;
								token = Shr;
						} else {
								token = Gt;
						}
						return ;
				} else if (token == '|') {
						if(*src == '|') {
								src ++;
								token = Lor;
						} else {
								token = Or;
						}
						return ;
				} else if(token == '&') {
						if(*src == '&') {
								src ++;
								token = Lan;
						} else {
								token = And;
						}
						return ;
				} else if(token == '^') {
						token = Xor;
						return ;
				} else if(token == '%') {
						token = Mod;
						return ;
				} else if(token == '*') {
						token = Mul;
						return ;
				} else if(token == '[') {
						token = Brak;
						return ;
				} else if(token == '?') {
						token = Cond;
						return ;
				} else if(token == '~' || token == ';' || token == '{' || token == '}' || token == '(' || token == ')' || token == ']' || token == ',' || token == ':') {

						return ;
				}
		}

		return ;
}
//解析表达式
void expression(int level) {

}

//语法分析入口
void program() {
		next();
		while(token > 0) {
				printf("token is: %c\n", token);
				next();
		}
}

//虚拟机入口，解释目标代码
int eval() {
		int op, *tmp;
		while(1) {
				op = *pc ++;
				if (op == IMM) {ax = *pc++;}
				else if (op == LC) {ax = *(char *)ax;}
				else if (op == LI) {ax = *(int *)ax;}
				else if (op == SC) {ax = *(char *)*sp ++ = ax;}
				else if (op == SI) {*(int *)*sp ++ = ax;}
				else if (op == PUSH) {*--sp = ax;}
				else if (op == JMP) {pc = (int *)*pc;}
				else if (op == JZ) {pc = ax ? pc + 1: (int *)*pc;}
				else if (op == JNZ) {pc = ax ? (int *)*pc: pc + 1;}
				else if (op == CALL) {*--sp = (int)(pc + 1); pc = (int *)*pc;}
				else if (op == ENT) {*--sp = (int)bp; bp = sp; sp = sp - *pc ++;}
				else if (op == ADJ) {sp = sp + *pc++;}
				else if (op == LEV) {sp = bp; bp = (int *)*sp ++; pc = (int *)*sp ++;}
				else if (op == LEA) {ax = (int)(bp + *pc ++);}
				else if (op == OR)  ax = *sp++ | ax;
				else if (op == XOR) ax = *sp++ ^ ax;
				else if (op == AND) ax = *sp++ & ax;
				else if (op == EQ)  ax = *sp++ == ax;
				else if (op == NE)  ax = *sp++ != ax;
				else if (op == LT)  ax = *sp++ < ax;
				else if (op == LE)  ax = *sp++ <= ax;
				else if (op == GT)  ax = *sp++ >  ax;
				else if (op == GE)  ax = *sp++ >= ax;
				else if (op == SHL) ax = *sp++ << ax;
				else if (op == SHR) ax = *sp++ >> ax;
				else if (op == ADD) ax = *sp++ + ax;
				else if (op == SUB) ax = *sp++ - ax;
				else if (op == MUL) ax = *sp++ * ax;
				else if (op == DIV) ax = *sp++ / ax;
				else if (op == MOD) ax = *sp++ % ax;
				else if (op == EXIT) { printf("exit(%d)", *sp); return *sp; }
				else if (op == OPEN) { ax = open((char *)sp[1], sp[0]);  }
				else if (op == CLOS) { ax = close(*sp); }
				else if (op == READ) { ax = read(sp[2], (char *)sp[1], *sp);  }
				else if (op == PRTF) { tmp = sp + pc[1]; ax = printf((char *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]);  }
				else if (op == MALC) { ax = (int)malloc(*sp); }
				else if (op == MSET) { ax = (int)memset((char *)sp[2], sp[1], *sp); }
				else if (op == MCMP) { ax = memcmp((char *)sp[2], (char *)sp[1], *sp); }
				else {
						printf("unknown istruction: %d\n", op);
						return -1;
				}
		}
		return 0;
}

int main(int argc, char **argv) {
		int i, fd;

		argc --;
		argv ++;

		poolsize = 256 * 1024;
		line = 1;
		if ((fd = open(*argv, 0)) < 0) {
				printf("could not open(%s)\n", *argv);
				return -1;
		}

		if(!(src = old_src = malloc(poolsize))) {
				printf("could not malloc(%d) for source area\n", poolsize);
				return -1;
		}

		if((i = read(fd, src, poolsize-1)) <= 0) {
				printf("read() returned %d\n", i);
				return -1;
		}

		src[i] = 0;
		close(fd);

		if (!(text = old_text = malloc(poolsize))) {
				printf("could not malloc(%d) for text area\n", poolsize);
				return -1;
		}

		if (!(data = malloc(poolsize))) {
				printf("could not malloc(%d) for data area\n", poolsize);
				return -1;
		}

		if (!(stack = malloc(poolsize))) {
				printf("could not malloc(%d) for stack area\n", poolsize);
				return -1;
		}

		memset(text, 0, poolsize);
		memset(data, 0, poolsize);
		memset(stack, 0, poolsize);

		bp = sp = (int *)((int)stack + poolsize);
		ax = 0;

		i = 0;
		text[i ++] = IMM;
		text[i ++] = 10;
		text[i ++] = PUSH;
		text[i ++] = IMM;
		text[i ++] = 20;
		text[i ++] = ADD;
		text[i ++] = PUSH;
		text[i ++] = EXIT;

		pc = text;
		
		//program();
		return eval();
}
