

from scipy.integrate import quadrature
import numpy as np
import math
import sys

#m   = int(sys.argv[1])
#rho = float(sys.argv[2])

def F0():
    return 0.5 * (math.pi / rho)**0.5 * math.erf(rho**0.5)

def func_re(x):
    y = np.exp(-rho*x*x) * x**(2*m)
    return y.real

def func_im(x):
    y = np.exp(-rho*x*x) * x**(2*m)
    return y.imag

#print(res_re, res_im)
#print(err_re, err_im)
#if(m==0):
#    print(F0())
#

eps = 1e-5

file1 = open('RESULTS.out', 'r')
Lines = file1.readlines()
for line in Lines:

    m     = int  (line.split()[0])
    z_re  = float(line.split()[1])
    z_im  = float(line.split()[2])
    z_mod = float(line.split()[3])

    rho   = z_re + 1j * z_im 

    Ffo_re = float(line.split()[4])
    Ffo_im = float(line.split()[5])

    Fpy_re, err_re = quadrature( func_re, a = 0, b = 1, tol = 1.49e-15, rtol = 1.49e-15, maxiter = 500000 )
    Fpy_im, err_im = quadrature( func_im, a = 0, b = 1, tol = 1.49e-15, rtol = 1.49e-15, maxiter = 500000 )
    
    diff_re = abs(Fpy_re - Ffo_re)
    diff_im = abs(Fpy_im - Ffo_im)
  
    if( ((diff_re > eps) or (diff_im > eps)) and (z_mod < 150) ):
        print(" m = {}, z = {} + {}j, |z| = {}".format(m, z_re, z_im, z_mod) )
        print(" fortran: {} + {}j, {}".format(Ffo_re, Ffo_im, (Ffo_re**2 + Ffo_im**2)**0.5) )
        print(" python : {} + {}j, {}".format(Fpy_re, Fpy_im, (Fpy_re**2 + Fpy_im**2)**0.5) )
        print(" error  = {} + {}j, {} \n".format(diff_re, diff_im, (diff_re**2 + diff_im**2)**0.5) )


