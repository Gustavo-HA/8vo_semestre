from mpl_toolkits import mplot3d
import math as ma
import random as rd
import scipy as sc
import scipy.special
import numpy as np
import matplotlib 
import matplotlib.pyplot as plt


a = 0.0529*10**(-9)


def alp(x,n,k):
    return sc.special.assoc_laguerre(x, n, k)


def pla(m,l,z):
    pla1 = sc.special.lpmn(m,l,z)
    return pla1[0][m][l]

def wavef(n,l,m,x,y,z):
    r = ma.sqrt(x**2+y**2+z**2)
    if (x>0 and y>0):
        phi = ma.atan(y/x)
    elif (x<0 and y>0) or (x<0 and y<0):
        phi = ma.atan(y/x) + ma.pi
    else:
        phi = ma.atan(y/x) + 2*ma.pi
    theta = ma.acos(z/r)
    
    f1 = ma.sqrt((2/(n*a))**3)  
    f2 = ma.sqrt(ma.factorial(n-l-1)/(2*n*(ma.factorial(n+l))**3))
    f3 = ma.e**(-r/(n*a))
    f4 = ((2*r)/(n*a))**l
    arm = ma.sqrt(((2*l+1)*ma.factorial(l-m))/(4*ma.pi*ma.factorial(l+m)))
    leg = pla(m,l,ma.cos(theta))
    lag = alp(2*r/(n*a),n-l-1,2*l+1)
    total = f1**2*f2**2*f3**2*f4**2*leg**2*arm**2*lag**2
    return total

#Obtenemos el máximo de las probabilidades#
n1 = int(input('Número cuántico n:',))
n2 = int(input('Número cuántico l:',))
n3 = int(input('Número cuántico m:',))

x = np.linspace(-n1**2*a-2*a,n1**2*a+2*a,50)
y = np.linspace(-n1**2*a-2*a,n1**2*a+2*a,50)
z = np.linspace(-n1**2*a-2*a,n1**2*a+2*a,50)
aux = 0
coord = np.array([0,2,4,1])
dv = (x[3]-x[2])**3

for i in range(0,len(x)):
    for j in range(0,len(y)):
        for k in range(0,len(z)):
            naux = wavef(n1,n2,n3,x[i],y[j],z[k])*dv
            if (naux > aux):
                aux = naux
                coord = np.delete(coord,(1,2,3))
                coord = np.append(coord,[x[i],y[j],z[k]])

#Obtenemos las coordenadas por muestreo#

ncoord=np.array([[0,0,0]])
p=1
while p<10000:
        rc = [[rd.uniform(-n1**2*a-2*a,n1**2*a+2*a),
               rd.uniform(-n1**2*a-2*a,n1**2*a+2*a),
               rd.uniform(-n1**2*a-2*a,n1**2*a+2*a)]]
        rn = rd.uniform(0,aux)
        if rn<=(wavef(n1,n2,n3,rc[0][0],rc[0][1],rc[0][2])*dv):
            ncoord=np.append(ncoord,rc, axis = 0)
            p = p+1

#HORA DE GRAFICAR POR FIN LA PTM
fig = plt.figure()
ax = plt.axes(projection = "3d")
ax.scatter(ncoord[:,0],ncoord[:,1],ncoord[:,2], marker =".", c="#702FDC")
plt.show()


