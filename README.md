# CMMC

**C Minus Minus Compiler (c--)** 编译器，计划实现一个基于C的能够自举的编译器

进度
 + 语法分析 
 + 词法分析
 + 语义分析

完成了自举
使用方法

```
gcc -o cmmc cmmc.c
./cmmc hello.c 
./cmmc cmmc.c hello.c 
./cmmc cmmc.c cmmc.c
```
