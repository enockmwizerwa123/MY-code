library('lpSolve')
library("linprog")
# define parameters
obj.fun<-c(1,5)
constr<-matrix(c(-1,3,1,1,1,-1),ncol = 2,byrow = TRUE)
constr
constr.dir<-c("<=","<=","<=")
rhs<-c(10,6,2)
##Solve the problem
prod.sol<-lp("max",obj.fun,constr,constr.dir,rhs,compute.sens = TRUE)
prod.sol
prod.sol$solution#Decision variables
prod.sol$duals#include dual constraints and reduce costs
#sensitivity analysis
prod.sol$duals.from
prod.sol$duals.to
prod.sol$sens.coef.from
prod.sol$sens.coef.to
########################## Exercise1 #########################

z<-c(1,1)
const<-matrix(c(4,-6,-5,-4,3,-2,4,1,8,-3,3,2),ncol =,byrow = TRUE)
const1
const.dire2<-c(">=","<=","<=")
RHS<-c(-20,11,23)
Ans1<-lp("max",F,const1,const.dire2,RHS,compute.sens = TRUE)
show(Ans1)
Ans1$solution
####################### Exercise 2 ###########################
F<-c(4,1,3,5)
const1<-matrix(c(-4,6,5,-4,3,-2,4,1,8,-3,3,2),ncol = 4,byrow = TRUE)
const1
const.dire2<-c("<=","<=","<=")
RHS1<-c(20,11,23)
Ans1<-lp("max",F,const1,const.dire2,RHS1,compute.sens = TRUE)
show(Ans1)
Ans$solution
############## Exercise 3 ###################################
bj.f<-c(-1,3,-3)
const2<-matrix(c(3,-1,1,-1,2,0,-4,3,8),ncol =3,byrow = TRUE)
const2
const.dire3<-c("<=","<=","<=")
RHS<-c(7,6,10)
Ans3<-lp("max",bj.f,const2,const.dire3,RHS,compute.sens = TRUE)
show(Ans3)
Ans3$solution
##################  Exercise 4###############################
g<-c(1,-3,3)
const4<-matrix(c(-1,1,-1,2,-1,1,-1,-1,1),ncol =3,byrow = TRUE)
const4
const.dire4<-c("<=","<=","<=")
RHS4<-c(2,2,2)
Ans4<-lp("max",g,const4,const.dire4,RHS4,compute.sens = TRUE)
show(Ans4)
Ans4$solution
################ Exercise 5#######################
Obj.f<-c(1,1,1,1,1)
const5<-matrix(c(3,2,1,0,0,5,1,1,1,0,2,5,1,0,1),ncol =5,byrow = TRUE)
const5
const.dire5<-c("=","=","=")
RHS5<-c(1,2,4)
Ans5<-lp("max",Obj.f,const5,const.dire5,RHS5,compute.sens = TRUE)
show(Ans5)
Ans5$solution

################### Exercise 6 ##################################

Obj.f6<-c(3,-3,-1,1,-1,1)
const6<-matrix(c(1,-1,2,-2,4,-4,1,-1,-1,1,-1,1),ncol =6,byrow = TRUE)
const6
const.dire6<-c("<=","<=")
RHS6<-c(5,8)
Ans6<-lp("max",Obj.f6,const6,const.dire6,RHS6,compute.sens = TRUE)
show(Ans6)
Ans6$solution

################Exercise7#######################################

Obj.f7<-c(-1,2,-1)
const7<-matrix(c(3,-1,2,0,-2,4,-4,3,8),ncol =3,byrow = TRUE)
const7
const.dire7<-c("<=","<=","<=")
RHS7<-c(7,8,1)
Ans7<-lp("max",Obj.f7,const7,const.dire7,RHS7,compute.sens = TRUE)
show(Ans7)
Ans7$solution
################ Exercise 8#################################
Obj.f8<-c(4,6,7,8)
const8<-matrix(c(1,1,1,1,0,0,0,1,2,3,4,7,3,4,5,6),ncol =4,byrow = TRUE)
const8
const.dire8<-c("=",">=","<=","<=")
RHS8<-c(950,400,4600,5000)
Ans8<-lp("max",Obj.f8,const8,const.dire8,RHS8,compute.sens = TRUE)
show(Ans8)
Ans8$solution

##################### Exercise 9 ###################
Obj.f9<-c(15,10,9,7)
const9<-matrix(c(1,1,1,1,0,0,1,0,2,3,4,5,3,4,5,6),ncol =4,byrow = TRUE)
const9
const.dire9<-c("=",">=","<=","<=")
RHS9<-c(1000,400,3300,4000)
Ans9<-lp("min",Obj.f9,const9,const.dire9,RHS9,compute.sens = TRUE)
show(Ans9)
Ans9$solution
####################### Exercise 10 ##############################
Obj.f10<-c(9,8,5,4)
const10<-matrix(c(1,0,0,1,0,1,1,0,1,1,1,0,2,1,1,1),ncol =4,byrow = TRUE)
const10
const.dire10<-c("<=","<=","<=","<=")
RHS10<-c(200,150,350,550)
Ans10<-lp("max",Obj.f10,const10,const.dire10,RHS10,compute.sens = TRUE)
show(Ans10)
Ans10$solution
########################## Exercise on integer programming##########################
M<-c(8,11,6,4)
f.con<-matrix(c(5,7,0,3,8,0,4,4,2,10,6,4),nrow = 3,byrow=TRUE)
f.dir<-c("<=","<=","<=")
f.RH
  ###############################################################################################

# Set coefficients of the objective function
f.obj <- c(8, 11, 6, 4)

# Set matrix corresponding to coefficients of constraints by rows
f.con <- matrix(c(5, 7, 0, 3,
                  8, 0, 4, 4,
                  2, 10, 6, 4), nrow = 3, byrow = TRUE)

# Set unequality/equality signs
f.dir <- c("<=",
           "<=",
           "<=")

# Set right hand side coefficients
f.rhs <- c(14,
           12,
           15)

# Final value (z)
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, all.bin = TRUE)

# Variables final values
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, all.bin = TRUE)$solution

# Sensitivities
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, compute.sens = TRUE, all.bin = TRUE)$sens.coef.from
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, compute.sens = TRUE, all.bin = TRUE)$sens.coef.too

# Dual Values (first dual of the constraints and then dual of the variables)
# Duals of the constraints and variables are mixed
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, compute.sens = TRUE, all.bin = TRUE)$duals

# Duals lower and upper limits:
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, compute.sens = TRUE, all.bin = TRUE)$duals.from
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, compute.sens = TRUE, all.bin = TRUE)$duals.to
obj.f<-c(1,5)
con<-matrix(c(-1,3,1,1,1,-1),ncol = 2,byrow = TRUE)
con.dir<-c("<=","<=","<=")
con.rhs<-c(10,6,2)
sol<-lp("max",obj.f,con,con.dir,con.rhs,compute.sens = TRUE)
sol$solution
sol
#################### Exercise 8############ Use simplex method ############
obj.g<-c(-1,3,-3)
b<-c(7,6,10)
A<-rbind(c(3,-1,1),c(-1,2,0),c(-4,3,8))
res<-solveLP(obj.g,b,A,maximum = TRUE)
print(res)
res$solution
################ Exercise 7#######################
obj.z<-c(1,1)
cond<-matrix(c(1,1,1,-1),ncol = 2,byrow = TRUE)
cond.rhs<-c(4,5)
cond.dir<-c("<=",">=")
solu<-lp("max",obj.z,cond,cond.dir,cond.rhs,compute.sens = TRUE)
solu
######################plotting a 3d graph#######################
cone<-function(x,y){sqrt(x^2+y^2)}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,cone)
par(mfrow=c(1,2))
persp(x,y,z,main="3d plot of sqrt(x^2+y^2)",zlab="height",
      theta=30,phi=15,
      shade=0.5)
grid()
persp(x,y,z,
main="3d plot of sqrt(x^2+y^2)",
zlab="height",
theta=30,phi=15,
col="springgreen",shade=0.5)
grid()
####################### Assessment###########################
#1
cone<-function(x,y){sqrt(x^2+y^2)}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,cone)
par(mfrow=c(1,2))
persp(x,y,z,main="3d plot of sqrt(x^2+y^2)")
grid()
persp(x,y,z,
      main="3d plot of sqrt(x^2+y^2)",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#2
f1<-function(x,y){(2*x^2-y^2)^2+3*x^2-y^2}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f1)
par(mfrow=c(1,2))
persp(x,y,z,main="3d plot of 2*x^2-y^2)^2+3*x^2-y^2)", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of 2*x^2-y^2)^2+3*x^2-y^2",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#3
f2<-function(x,y){(x^2-y)^2+(x-1)^2}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f2)
par(mfrow=c(1,2))
persp(x,y,z,main="3d plot of (x^2-y)^2+(x-1)^2)", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of (x^2-y)^2+(x-1)^2",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#4
f3<-function(x,y){x^2-6*x+2*y^2-8*y}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f3)
par(mfrow=c(1,2))
persp(x,y,z,main="3d plot of x^2-6*x+2*y^2-8*y", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of x^2-6*x+2*y^2-8*y",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#5
f4<-function(x,y){x^3-x^2+4*x+2*y^2+2*y}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f4)
par(mfrow=c(1,2))
persp(x,y,z,main="3d plot of x^3-x^2+4*x+2*y^2+2*y", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of x^3-x^2+4*x+2*y^2+2*y",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()

#6
f5<-function(x,y){x^3+y^2-2*x*y+6*x-2*y}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f5)
par(mfrow=c(1,5))
persp(x,y,z,main="3d plot of x^3+y^2-2*x*y+6*x-2*y", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of x^3+y^2-2*x*y+6*x-2*y",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#7
f6<-function(x,y){-x^2+4*y^2+x*y}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f6)
par(mfrow=c(1,5))
persp(x,y,z,main="3d plot of -x^2+4*y^2+x*y", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of -x^2+4*y^2+x*y",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#8
f7<-function(x,y){x^4+y^2+2*y^2+x*y}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f7)
par(mfrow=c(1,5))
persp(x,y,z,main="3d plot of x^4+y^2+2*y^2+x*y", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of x^4+y^2+2*y^2+x*y",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#9
f8<-function(x,y){-x^2+2*y^2-4*x*y}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f8)
par(mfrow=c(1,5))
persp(x,y,z,main="3d plot of -x^2+2*y^2-4*x*y", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of -x^2+2*y^2-4*x*y",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#10
f9<-function(x,y){(1/12)*x^4-x^2+y^2-2*x*y-2*x}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f9)
par(mfrow=c(1,5))
persp(x,y,z,main="3d plot of (1/12)*x^4-x^2+y^2-2*x*y-2*x", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of (1/12)*x^4-x^2+y^2-2*x*y-2*x",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#11
f10<-function(x,y){100*(y-x^2)^2-(1-x)^2}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f10)
par(mfrow=c(1,5))
persp(x,y,z,main="3d plot of 100*(y-x^2)^2-(1-x)^2", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of 100*(y-x^2)^2-(1-x)^2",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#12

f11<-function(x,y){exp({x^2+y^2})+2*x^2+4*y}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f11)
par(mfrow=c(1,5))
persp(x,y,z,main="3d plot of e^(x^2+y^2)+2&x^2+4*y", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of e^(x^2+y^2)+2&x^2+4*y",
      zlab="height",
      theta=30,phi=15,
      col="springgreen",shade=0.5)
grid()
#13
f12<-function(x,y){2*x^2+y^2-x*y-y}
x<-y<-seq(-1,1,length=20)
z<-outer(x,y,f12)
par(mfrow=c(1,5))
persp(x,y,z,main="3d plot of 2*x^2+y^2=x*y-y", zlab="height",
      theta=30,phi=15,)
grid()
persp(x,y,z,
      main="3d plot of 2*x^2+y^2=x*y-y",
      zlab="height",
      theta=30,phi=15,
      col="red",shade=0.5)
grid()
####################### exercise 1#######################
#a
obj.z<-c(-1,2)
cond.z<-matrix(c(-1,1,1,3,1,-1),ncol = 2,byrow = TRUE)
con.dirz<-c("<=","<=","<=")
con.rhz<-c(10,6,2)
solt<-lp("min",obj.z,cond.z,con.dirz,con.rhz,compute.sens = TRUE)
solt
solt$solution
#b
obj.p<-c(3,4)
cond.p<-matrix(c(1,-1,-2,2),ncol = 2,byrow = TRUE)
con.dirp<-c("<=",">=")
con.rhp<-c(-1,0)
soltp<-lp("max",obj.p,cond.p,con.dirp,con.rhp,compute.sens = TRUE)
soltp$solution
soltp
#c
obj.k<-c(1,1,1,1,1,1,1)
cond.k<-matrix(c(1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1),ncol = 7,byrow = TRUE)

cond.k
con.dirk<-c(">=",">=",">=",">=",">=",">=",">=")
con.rhk<-c(17,13,15,19,14,16,11)
soltk<-lp("min",obj.k,cond.k,con.dirk,con.rhk,compute.sens = TRUE)
soltk$solution
soltk
########################### Assessment######################
#q1
obj.h<-c(5,2)
con.h<-matrix(c(3,1,1,1),nrow = 2)
cn.dh<-c("<=","<=")
hrh<-c(12,5)
v<-lp("max",obj.h,con.h,cn.dh,hrh,all.int = T,compute.sens = TRUE)
v
v$solution
#2
obj.1<-c(50,100)
cond.1<-matrix(c(7,2,2,12),ncol = 2,byrow = TRUE)
con.dir1<-c(">=",">=")
con.rh1<-c(28,24)
solt1<-lp("min",obj.1,cond.1,con.dir1,con.rh1,compute.sens = TRUE)
solt1$solution
solt1
#q3

obj.u<-c(2,3)
con.u<-matrix(c(1,3,2,4),nrow = 2)
con.u
cn.du<-c("<=","<=")
hru<-c(10,25)
u<-lp("max",obj.u,con.u,cn.du,hru,all.int = T,compute.sens = TRUE)
u
u$solution
#q4
obj.o<-c(4,3)
con.o<-matrix(c(4,8,9,5),nrow = 2)
con.o
cn.do<-c("<=","<=")
hro<-c(26,17)
o<-lp("max",obj.o,con.o,cn.do,hro,all.int = T,compute.sens = TRUE)
o
o$solution
#q4
obj.c<-c(4,5)
con.c<-matrix(c(1,3,4,2),nrow = 2)
con.c
cn.dc<-c(">=",">=")
hrc<-c(5,7)
c<-lp("max",obj.c,con.c,cn.dc,hrc,all.int = T,compute.sens = TRUE)
c
c$solution
#q6
obj.s<-c(4,5)
con.s<-matrix(c(3,1,3,2,4,3),nrow = 3)
con.s
cn.s<-c("<=","<=","<=")
hrs<-c(10,11,13)
s<-lp("max",obj.s,con.s,cn.s,hrs,all.int = T,compute.sens = TRUE)
s
s$solution
#q7
obj.e<-c(7,3)
con.e<-matrix(c(2,3,1,2),nrow = 2)
con.e
cn.de<-c("<=","<=")
hre<-c(9,13)
e<-lp("max",obj.e,con.e,cn.de,hre,all.int = T,compute.sens = TRUE)
e
e$solution
############################### Exercise2 ################################################
#a
obj<-c(3,2)
con<-matrix(c(2,4,5,2),nrow = 2)
cn<-c("<=","<=")
hr<-c(9,9)
w<-lp("max",obj,con,cn,hr,all.int = T,compute.sens = TRUE)
w
w$solution

#b
obj1<-c(2,3)
con1<-matrix(c(5,4,7,9),nrow = 2)
cn1<-c("<=","<=")
hr1<-c(35,36)
w1<-lp("max",obj1,con1,cn1,hr1,all.int = T,compute.sens = TRUE)
w1
w1$solution
#c
obj2<-c(1,1)
con2<-matrix(c(2,6,5,5),nrow = 2)
cn2<-c("<=","<=")
hr2<-c(16,27)
w2<-lp("max",obj2,con2,cn2,hr2,all.int = T,compute.sens = TRUE)
w2
w2$solution
#d
obj3<-c(5,4)
con3<-matrix(c(3,2,2,3),nrow = 2)
cn3<-c("<=","<=")
hr3<-c(5,7)
w3<-lp("max",obj3,con3,cn3,hr3,all.int = T,compute.sens = TRUE)
w3
w3$solution
#e
obj4<-c(5,7)
con4<-matrix(c(2,5,1,9),nrow = 2)
cn4<-c("<=","<=")
hr4<-c(13,41)
w4<-lp("max",obj4,con4,cn4,hr4,all.int = T,compute.sens = TRUE)
w4
w4$solution

####################### Ip Problems####################
#Task 1
obj.Z<-c(40,80,10,10,4,20,60)
conZ<-matrix(c(40,50,30,10,10,40,30),nrow = 1,byrow = TRUE)
conZ
condZ<-c("<=")
rhZ<-c(100)
asd<-lp("max",obj.Z,conZ,condZ,rhZ,all.bin = T,compute.sens = TRUE)
asd
asd$solution
#task 2
obj.F<-c(1,1,1,1,1,1)
conF<-matrix(c(1,1,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,1,0,0,1,1),ncol =6 , byrow = TRUE)
conF
condF<-c(">=",">=",">=",">=",">=",">=")
rhF<-c(1,1,1,1,1,1)
asF<-lp("min",obj.F,conF,condF,rhF,all.bin = T,compute.sens = TRUE)
asF
asd$solution
#Task3
Obj.A<-c(28,84,112,112,60,20,50,50,96,60,24,60,64,40,40,16,50,50,50,50)
Cond.A<-matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,-1      ),ncol = 20,byrow = TRUE)   
Cond.A
cond.A1<-c("=","=","=","=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=","<=")
RHA<-c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
asA<-lp("min",Obj.A,Cond.A,cond.A1,RHA,all.bin = T,compute.sens = TRUE)
asA
asA$solution
#task 4
Obj.B<-c(14,5,8,7,2,12,6,5,7,8,3,9,2,4,6,10)
cond.B<-matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1),ncol = 16,byrow = TRUE)
cond.B
cond.B1<-c("=","=","=","=","=","=","=","=","=","=","=","=","=","=","=","=")
RHB<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
asB<-lp("min",Obj.B,cond.B,cond.B1,RHB,all.bin = T,compute.sens = TRUE)
asB
asB$solution





