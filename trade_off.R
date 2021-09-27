x <- seq(1,7,1)
b0 <- 1
b1 <- 0.2
b2 <- 0.3
b3 <- 0.4
y1 <- b0 + b1*x
y2 <- b0 + b1*x +b2*x^2
y3 <- b0 + b1*x +b2*x^2 + b3*x^3
plot(y1)
plot(y2)
plot(y3)



n = 100  #生成100个模拟数据
target.fun = function(x) 5 * sin(2 * x) + 2 * cos(x + 1)
x = runif(n, 0, 6)
y = target.fun(x) + rnorm(n, 0, 1)  #增加一个噪音
grid.x = data.frame(x = seq(0, 6, .01))  #生成网格，为了画图
plot(x, y, pch = "+", col = rgb(0, 0, 0, alpha = .5)) #观测
lines(grid.x$x, target.fun(grid.x$x), col = "red", lwd = 2)  #真实



B = 1000 # number of simulations
mse = function(y, y.hat) mean((y - y.hat)^2)
k = 12 # maximum degree of the polynomial we fit (model complexity)
ratio = .5 # train / test set split ratio 
train.indices = c(rep(TRUE, ratio * n), rep(FALSE, (1 - ratio) * n)) #50个true 50个false
train.errors = matrix(nrow = k, ncol = B)   #用来存结果的，k*b的矩阵
test.errors = matrix(nrow = k, ncol = B)   #用来存结果的
for (b in (1:B)) {
  x = runif(n, 0, 6) #100个
  y = target.fun(x) + rnorm(n, 0, 1) #函数+误差项
  train.df = data.frame(x = x[train.indices], y = y[train.indices]) #开始构建训练数据：把取值为true的50个x取出来，y同理
  test.df = data.frame(x = x[!train.indices], y = y[!train.indices]) #！表示非的符号，即为false
  for (i in (1:k)) {
    fit = lm(y ~ poly(x, i, raw = TRUE), data = train.df)    #poly表示多项式函数,i=1则为线性，i=2为二阶，依此类推
    train.errors[i, b] = mse(train.df$y, predict(fit))
    test.errors[i, b] = mse(test.df$y, predict(fit, newdata = test.df))
  }  #填完第一列
} #填完整个12*1000

#整理数据，为了作图
p.data = data.frame(k = rep(1:k, 2), error = c(rowMeans(train.errors), rowMeans(test.errors)), #1000个均值
                    grp = c(rep("train", k), rep("test", k)))   #grp 用来作为标签 
library(ggplot2)
ggplot(data = p.data, mapping = aes(x = factor(k), y = error, group = grp, color = grp)) +
  geom_point(size = 2) + geom_line(alpha = .2) + coord_cartesian(ylim = c(0, 30)) +
  xlab("Model Complexity") + ylab("Prediction Error") + 
  theme(legend.title = element_blank(), legend.position = c(.5, .7))


#一开始是在减小bias；到了9以后，在test上MSE迅速增加，所以是方差增大的部分
# sweet point 在中间7的位置，即阶数为7