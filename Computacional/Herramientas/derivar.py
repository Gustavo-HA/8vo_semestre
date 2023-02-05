from sympy import *
x = symbols('x')
f = exp(2*x)/x**2
df = diff(f,x)
print(f)
print(df)
df = lambdify(x,df)
f = lambdify(x,f)

print(f(2.0))
print(df(2.0))
