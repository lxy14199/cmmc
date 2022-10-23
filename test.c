#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>

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
enum {  Num = 128, Fun, Sys, Glo, Loc, Id, 
		Char, Else, Enum, If, Int, Return, Sizeof, While, 
		Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc,Dec, Brak
};

//结构体
enum {Token, Hash, Name, Type, Class, Value, BType, BClass, BValue, IdSize};  //等价于定义了identifier结构体
int token_value;
int *current_id,
	*symbols;
enum {CHAR, INT, PTR};
//递归下降解析
int basetype;  
int expr_type;
int *idmain;
int index_of_bp;

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
						if (token_value > 0) {
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

void mattch(int tk) {
		if(token == tk) {
				next();
		} else {
				printf("%d: expected token: %d\n", line, token);
				exit(-1);
		}
}

//解析表达式
void expression(int level) {
		int *id;
		int tmp;
		int *addr;

		{
				if(token == Num) {
						mattch(Num);
						*++text = IMM;
						*++text = token_value;
						expr_type = INT;
				} else if(token == '"') {
						*++text = IMM;
						*++text = token_value;

						mattch('"');

						while (token == '"') {
								mattch('"');
						}

						data = (char *)(((int)data + sizeof(int)) & (-sizeof(int)));
						expr_type = PTR;
				} else if (token == Sizeof) {
						mattch(Sizeof);
						mattch('(');
						expr_type = INT;

						if(token == Int){
								mattch(Int);
						} else if (token == Char) {
								mattch(Char);
								expr_type = CHAR;
						} 

						while (token == Mul) {
								mattch(Mul);
								expr_type = expr_type + PTR;
						}

						mattch(')');

						*++text = IMM;
						*++text = (expr_type == CHAR) ? sizeof(char) : sizeof(int);

						expr_type = INT;
				} else if(token == Id) {
						mattch(Id);

						id = current_id;

						if (token == '(') {
								mattch('(');

								tmp = 0;
								while (token != ')') {
										expression(Assign);
										*++text = PUSH;
										tmp ++;

										if(token == ',') {
												mattch(',');
										}
								}

								mattch(')');

								if(id[Class] == Sys) {
										*++text = id[Value];
								} else if (id[Class] == Fun) {
										*++text = CALL;
										*++text = id[Value];
								} else {
										printf("%d: bad function call\n", line);
										exit(-1);
								}

								if(tmp > 0) {
										*++text = ADJ;
										*++text = tmp;
								}
								expr_type = id[Type];
						} else if(id[Class] == Num) {
								*++text = IMM;
								*++text = id[Value];
								expr_type = INT;
						} else {
								if(id[Class] == Loc) {
										*++text = LEA;
										*++text = index_of_bp - id[Value];
								} else if(id[Class] == Glo) {
										*++text = IMM;
										*++text = id[Value];
								} else {
										printf("%d: undefined variable\n", line);
										exit(-1);
								}

								expr_type = id[Type];
								*++text = (expr_type == Char) ? LC : LI;
						}
				} else if(token == '(') {
						mattch('(');
						if(token == Int || token == Char) {
								tmp = (token == Char) ? CHAR : INT;
								mattch(token);

								while (token == Mul) {
										mattch(Mul);
										tmp = tmp + PTR;
								}

								mattch(')');

								expression(Inc);

								expr_type = tmp;
						} else {
								expression(Assign);
								mattch(')');
						}
				} else if(token == Mul) {
						mattch(Mul);
						expression(Inc);

						if(expr_type >= PTR) {
								expr_type = expr_type - PTR; 
						} else {
								printf("%d: bad derefreence\n", line);
								exit(-1);
						}

						*++text = (expr_type == CHAR) ? LC : LI;
				} else if(token == And) {
						mattch(And);
						expression(Inc);
						if(*text == LC || *text == LI) {
								text --;
						} else {
								printf("%d: bad address of\n", line);
								exit(-1);
						}

						expr_type = expr_type + PTR;
				} else if(token == '!') {
						mattch('!');
						expression(Inc);

						*++text = PUSH;
						*++text = IMM;
						*++text = 0;
						*++text = EQ;

						expr_type = INT;
				} else if(token == '~') {
						mattch('~');
						expression(Inc);

						*++text = PUSH;
						*++text = IMM;
						*++text = XOR;

						expr_type = INT;
				} else if(token == Add) {
						mattch(Add);
						expression(Inc);

						expr_type = INT;
				} else if(token == Sub) {
						mattch(Sub);

						if(token == Num) {
								*++text = IMM;
								*++text = -token_value;
								mattch(Num);
						} else {
								*++text = IMM;
								*++text = -1;
								*++text = PUSH;
								expression(Inc);
								*++text = MUL;
						}

						expr_type = INT;
				} else if(token == Inc || token == Dec) {
						tmp = token;
						mattch(token);
						expression(Inc);

						if(*text == LC) {
								*text = PUSH;
								*++text = LC;
						} else if(*text == LI) {
								*text = PUSH;
								*++text = LI;
						} else {
								printf("%d: bad lvalue of pre-increment\n", line);
								exit(-1);
						}

						*++text = PUSH;
						*++text = IMM;

						*++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
						*++text = (tmp == Inc) ? ADD : SUB;
						*++text = (expr_type == CHAR) ? SC : SI;
				} else {
						printf("%d: bad expression\n", line);
						exit(-1);
				} 
		}

		{
				while (token >= level) {
						tmp = expr_type;
						if(token == Assign) {
								mattch(Assign);
								if (*text == LC || *text == LI) {
										*text = PUSH;
								} else {
										printf("%d: bad lvalue in assignment!\n", line);
										exit(-1);
								}
								expression(Assign);
								expr_type = tmp;

								*++text = (expr_type == CHAR) ? SC : SI;
						} else if (token == Cond) {
								mattch(Cond);
								*++text = JZ;
								addr = ++text;
								expression(Assign);
								if (token == ':') {
										mattch(':');
								} else {
										printf("%d: missing colon in conditional\n", line);
										exit(-1);
								}
								*addr = (int)(text + 3);
								*++text = JMP;
								addr = ++text;
								expression(Cond);
								*addr = (int)(text + 1);
						} else if(token == Lor) {
								mattch(Lor);
								*++text = JNZ;
								addr = ++text;
								expression(Lan);
								*addr = (int)(text + 1);
								expr_type = INT;
						} else if(token == Lan) {
								mattch(Lan);
								*++text = JZ;
								addr = ++text;
								expression(Or);
								*addr = (int)(text + 1);
								expr_type = INT;
						} else if (token == Or) {
								mattch(Or);
								*++text = PUSH;
								expression(Xor);
								*++text = OR;
								expr_type = INT;
						} else if (token == Xor) {
								mattch(Xor);
								*++text = PUSH;
								expression(And);
								*++text = XOR;
								expr_type = INT;
						} else if (token == And) {
								mattch(And);
								*++text = PUSH;
								expression(Eq);
								*++text = AND;
								expr_type = INT;
						} else if (token == Eq) {
								mattch(Eq);
								*++text = PUSH;
								expression(Ne);
								*++text = EQ;
								expr_type = INT;
						} else if (token == Ne) {
								mattch(Ne);
								*++text = PUSH;
								expression(Lt);
								*++text = NE;
								expr_type = INT;
						} else if (token == Lt) {
								mattch(Lt);
								*++text = PUSH;
								expression(Shl);
								*++text = LT;
								expr_type = INT;
						} else if (token == Gt) {
								mattch(Gt);
								*++text = PUSH;
								expression(Shl);
								*++text = GT;
								expr_type = INT;
						} else if (token == Le) {
								mattch(Le);
								*++text = PUSH;
								expression(Shl);
								*++text = LE;
								expr_type = INT;
						} else if (token == Ge) {
								mattch(Ge);
								*++text = PUSH;
								expression(Shl);
								*++text = GE;
								expr_type = INT;
						} else if (token == Shl) {
								mattch(Shl);
								*++text = PUSH;
								expression(Add);
								*++text = SHL;
								expr_type = INT;
						} else if (token == Shr) {
								mattch(Shr);
								*++text = PUSH;
								expression(Add);
								*++text = SHR;
								expr_type = INT;
						} else if (token == Add) {
								mattch(Add);
								*++text = PUSH;
								expression(Mul);

								expr_type = tmp;
								if (expr_type > PTR) {
										*++text = PUSH;
										*++text = IMM;
										*++text = sizeof(int);
										*++text = MUL;
								}
								*++text = ADD;
						} else if (token == Sub) {
								mattch(Sub);
								*++text = PUSH;
								expression(Mul);
								if (tmp > PTR && tmp == expr_type) {
										*++text = SUB;
										*++text = PUSH;
										*++text = IMM;
										*++text = sizeof(int);
										*++text = DIV;
										expr_type = INT;
								} else if (tmp > PTR) {
										*++text = PUSH;
										*++text = IMM;
										*++text = sizeof(int);
										*++text = MUL;
										*++text = SUB;
										expr_type = tmp;
								} else {
										*++text = SUB;
										expr_type = tmp;
								}
						} else if (token == Mul) {
								mattch(Mul);
								*++text = PUSH;
								expression(Inc);
								*++text = MUL;
								expr_type = tmp;
						} else if (token == Div) {
								mattch(Div);
								*++text = PUSH;
								expression(Inc);
								*++text = DIV;
								expr_type = tmp;
						} else if (token == Mod) {
								mattch(Mod);
								*++text = PUSH;
								expression(Inc);
								*++text = MOD;
								expr_type = tmp;
						} else if (token == Inc || token == Dec) {
								if (*text == LI) {
										*text = PUSH;
										*++text = LI;
								} else if (*text == LC) {
										*text = PUSH;
										*++text = LC;
								} else {
										printf("%d: bad value in increment\n", line);
										exit(-1);
								}

								*++text = PUSH;
								*++text = IMM;
								*++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
								*++text = (token == Inc) ? ADD : SUB;
								*++text = (expr_type == CHAR) ? SC : SI;
								*++text = PUSH;
								*++text = IMM;
								*++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
								*++text = (token == Inc) ? SUB : ADD;
								mattch(token);
						} else if (token == Brak) {
								mattch(Brak);
								*++text = PUSH;
								expression(Assign);
								mattch(']');

								if (tmp > PTR) {
										*++text = PUSH;
										*++text = IMM;
										*++text = sizeof(int);
										*++text = MUL;
								} else if (tmp < PTR) {
										printf("%d: pointer type expected\n", line);
										exit(-1);
								}
								expr_type = tmp - PTR;
								*++text = ADD;
								*++text = (expr_type == CHAR) ? LC : LI;
						} else {
								printf("%d: compiler error, token = %d\n", line, token);
								exit(-1);
						}
				}
		}
}

//enum解析
void enum_declaration() {
		int i;
		i = 0;
		while (token != '}') {
				if(token != Id){
						printf("%d: bad enum declaration\n", line);
						exit(-1);
				}

				next();
				if (token == Assign) {
						next();
						if (token != Num) {
								printf("%d: bad enum num declaration\n", line);
								exit(-1);
						}
						i = token_value;
						next();
				}

				current_id[Class] = Num;
				current_id[Type] = INT;
				current_id[Value] = i ++;

				if(token == ',') {
						next();
				}

		}
}

//解析函数参数
void function_parameter() {
		int type;
		int params;
		params = 0;
		while (token != ')') {
				type = INT;
				if (token == Int) {
						mattch(Int);
				} else if (token == Char) {
						type = CHAR;
						mattch(Char);
				}

				while (token == Mul) {
						mattch(Mul);
						type = type + PTR;
				}
				if (token != Id) {
						printf("%d: bad parameter declaration\n", line);
						exit(-1);

				}
				if (current_id[Class] == Loc) {
						printf("%d: duplicate parameter declaration\n", line);
						exit(-1);

				}

				mattch(Id);
				current_id[BClass] = current_id[Class]; current_id[Class] = Loc;
				current_id[BType] = current_id[Type]; current_id[Type] = type;
				current_id[BValue] = current_id[Value]; current_id[Value] = params ++;

				if (token == ',') {
						mattch(',');
				}	
		}

		index_of_bp = params + 1;
}

//解析各种语句
void statement() {
		int *a, *b;

		if (token == If) {
				mattch(If);
				mattch('(');
				expression(Assign);
				mattch(')');

				*++text = JZ;
				b = ++text;

				statement();
				if (token == Else) {
						mattch(Else);

						*b = (int)(text + 3);
						*++text = JMP;
						b = ++text;

						statement();
				}
				*b = (int)(text + 1);
		} else if (token == While) {
				mattch(While);

				a = text + 1;
				mattch('(');
				expression(Assign);
				mattch(')');

				* ++text = JZ;
				b = ++ text;

				statement();

				*++text = JMP;
				*++text = (int)a;
				*b = (int)(text + 1);	
		} else if(token == Return) {
				mattch(Return);

				if (token != ';') {
						expression(Assign);
				}

				mattch(';');

				*++text = LEV;
		} else if(token == '{') {
				mattch('{');
				while (token != '}') {
						statement();
				}
				mattch('}');
		} else if(token == ';') {
				mattch(';');
		} else {
				expression(Assign);
				mattch(';');
		}
}

//解析函数体
void function_body() {
		int pos_local;
		int type;
		pos_local = index_of_bp;

		while (token == Int || token == Char) {
				basetype = (token == Int) ? INT : CHAR;
				mattch(token);

				while (token != ';') {
						type = basetype;
						while (token == Mul) {
								mattch(Mul);
								type = type + PTR;
						}
						if (token != Id) {
								printf("%d: bad parameter declaration\n", line);
								exit(-1);					        
						}
						if (current_id[Class] == Loc) {
								printf("%d: duplicate parameter declaration\n", line);
								exit(-1);						        
						}
						mattch(Id);
						current_id[BClass] = current_id[Class]; current_id[Class] = Loc;
						current_id[BType] = current_id[Type]; current_id[Type] = type;
						current_id[BValue] = current_id[Value]; current_id[Value] = ++ pos_local;

						if (token == ',') {
								mattch(',');
						}
				}
				mattch(';');
		}

		*++text = ENT;
		*++text = pos_local - index_of_bp;

		while (token != '}') {
				statement();
		}

		*++text = LEV;
}

//解析函数
void function_declaration() {
		mattch('(');
		function_parameter();
		mattch(')');

		mattch('{');
		function_body();

		current_id = symbols;
		while (current_id[Token]) {
				if (current_id[Class] == Loc) {
						current_id[Class] = current_id[BClass];
						current_id[Type] = current_id[BType];
						current_id[Value] = current_id[BValue];
				}
				current_id = current_id + IdSize;
		}
}

//解析全局变量，enum，函数
void global_declaration() {
		int type;
		int i;

		basetype = INT;

		if (token == Enum) {
				mattch(Enum);
				if(token != '{') {
						mattch(Id);
				} 
				if(token == '{') {
						mattch('{');
						enum_declaration();
						mattch('}');
				}

				mattch(';');
				return ;
		} 

		if(token == Int) {
				mattch(Int);
		} else if(token == Char) {
				mattch(Char);
				basetype = CHAR;
		} 

		while (token != ';' && token != '}') {
				type = basetype;
				while (token == Mul) {
						mattch(Mul);
						type = type + PTR;
				}
				if(token != Id) {
						printf("%d %d\n", token, Id);
						printf("%d: bad global declaration!\n", line);
						exit(-1);
				}
				if(current_id[Class]) {
						printf("%d: duplicate global declaration\n", line);
						exit(-1);
				}
				mattch(Id);
				current_id[Type] = type;

				if(token == '(') {
						current_id[Class] = Fun;
						current_id[Value] = (int)(text + 1);
						function_declaration();
				} else {
						current_id[Class] = Glo;
						current_id[Value] = (int)data;
						data = data + sizeof(int);
				}

				if(token == ',') {
						mattch(',');
				}
		}
		next();
}


//语法分析入口
void program() {
		next();
		while(token > 0) {
				global_declaration();
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
				else if (op == EXIT) { printf("exit(%d)\n", *sp); return *sp; }
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

#undef int

int main(int argc, char **argv) {
		#define int long long	
		int i, fd;
		int *tmp;
		argc --;
		argv ++;

		poolsize = 256 * 1024;
		line = 1;


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

		if(!(symbols = malloc(poolsize))) {
				printf("could not malloc(%d) for symbols area\n", poolsize);
				return -1;
		}

		memset(text, 0, poolsize);
		memset(data, 0, poolsize);
		memset(stack, 0, poolsize);
		memset(symbols, 0, poolsize);	
		bp = sp = (int *)((int)stack + poolsize);
		ax = 0;
		src = "char else enum if int return sizeof while "
				"open read close printf malloc memset memcmp exit void main";


		i = Char;
		while (i <= While) {
				next();
				current_id[Token] = i ++;
		}
		i = OPEN;
		while (i <= EXIT) {
				next();
				current_id[Class] = Sys;
				current_id[Type] = INT;
				current_id[Value] = i ++;
		}
		next(); current_id[Token] = Char;
		next(); idmain = current_id;

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
		program();

		if (!(pc = (int *)idmain[Value])) {
				printf("main() not defined\n");
				return -1;		    
		}

		sp = (int *)((int)stack + poolsize);
		*--sp = EXIT; // call exit if main returns
		*--sp = PUSH; tmp = sp;
		*--sp = argc;
		*--sp = (int)argv;
		*--sp = (int)tmp;
		
		return eval();
}
