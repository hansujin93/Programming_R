library(ggplot2)
r_det = 30
f_det = 40
w = 104
alph = 0.05
bet = 0.0001
gam = 0.02
for (t in 1:(w-1)) {
  r_det[t+1] = r_det[t] + alph*r_det[t] - bet*r_det[t]*f_det[t]
  f_det[t+1] = f_det[t] + bet*r_det[t]*f_det[t] - gam*f_det[t]
}
print(round(r_det[t+1]))
print(round(f_det[t+1]))
-----------------------------------------------------------------------------------------------------------------------------
set.seed(60854)
r_stoc = 30
f_stoc = 40
for (t in 1:(w-1)) {
  rv = rbinom(1, r_stoc[t]*f_stoc[t], bet)
  r_stoc[t+1] = r_stoc[t] + rbinom(1, r_stoc[t], alph) - rv
  f_stoc[t+1] = f_stoc[t] + rv - rbinom(1, f_stoc[t], gam)
}
print(round(r_stoc[t+1]))
print(round(f_stoc[t+1]))
-----------------------------------------------------------------------------------------------------------------------------
LV = data.frame(
  time = rep(1:w, 4),
  group = rep(c("det_rabbits","det_foxes","sto_rabbits","sto_foxes"), each=w),
  size = c(r_det, f_det, r_stoc, f_stoc)
)
ggplot(LV) +
  geom_line(aes(x=time, y=size, colour=group, linetype=group)) +
  theme(legend.position="top")
