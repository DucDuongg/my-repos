library("BSDA")

#1: biet sigma = 0.08 => dung z.test
pcb = c(11.2,12.4,10.8,11.6,12.5,10.1,12.2,10.6)

z.test(pcb, sigma.x = 0.08, conf.level = 0.95)

#2: biet co mau (n.x), tb mau (mean.x), biet sigma = 0.2 => dung zsum.test
zsum.test(n.x = 20, mean.x = 1.2, sigma.x = 0.2, conf.level = 0.99)

#3: biet co mau (n.x), tb mau (mean.x), ko biet sigma, co mau < 30, co gt phan phoi chuan => dung tsum.test
tsum.test(mean.x = 1.2, s.x = 0.04, n.x = 20, conf.level = 0.99)

#4: ko co sigma, co mau 24 < 30, co phan phoi chuan => dung t.test
x = c(330,322,345,328.6,331,342,342.4,340,329.7,334,326.5,325.8,337.5,327.3,322.6,341,340,333)
t.test(x, conf.level = 0.95)

#5: mle

# vi du voi pp nhi thuc
library("stats4")

str(mtcars)
length(mtcars$am)
x = mtcars$am

L = function(p, x){
  prod(dbinom(x, size = 1, prob = p))
}

optimize(L, interval = c(0,1), x, maximum = TRUE)

# vi du voi pp chuan
x = PlantGrowth$weight
NegativeLog = function(mu, sigma){
  -sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}

mle(NegativeLog, start = list(mu=mean(x), sigma = sd(x)))

# bai 5
#a: tim mle cua trung binh va phuong sai tong the
x = c(140,136,150,144,148,152,138,141,143,151)
mle(NegativeLog, start = list(mu=mean(x), sigma = sd(x)))

#b: tim confidence interval cho phuong sai tong the var(x)
# R ko co ham, phai tim theo cong thuc
x
df = length(x) - 1
varianceX = var(x)

# Cong thuc khoang tin cay : ( (n-1) * s^2 / Chi-square(1 - alpha/2) ; (n-1) * s^2 / Chi-square(alpha/2) )
lowerBound = varianceX*df/qchisq(0.005, df, lower.tail = FALSE)
upperBound = varianceX*df/qchisq(0.995, df, lower.tail = FALSE)

c(lowerBound, varianceX, upperBound)

# bai 7: prop.test
prop.test(x = 5016, n = 10000, conf.level = 0.99)

# bai 8: prop.test
prop.test(x = 16, n = 100, conf.level = 0.95)

# bai 9: biet sigma => z.test
x = c(8.18, 8.17, 8.16, 8.15, 8.17, 8.21, 8.22, 8.16, 8.19, 8.18)
z.test(x, alternative = "two.sided", mu = 8.2, sigma.x = 0.02, conf.level = 0.95)

# thay p-value = 0.0008989 < alpha = 0.1 => bac bo Ho, chap nhan H1: do pH khac 8.2

# bai 10: biet sigma => z.test
x = c(210, 198, 195, 202, 197.5, 196, 199, 195.5)
z.test(x, alternative = "two.side", mu = 200, sigma.x = 5, conf.level = 0.95)

# thay p-value = 0.6206 > alpha = 0.05 => chua du co so bac bo Ho, tam chap nhan rang loai soi du tieu chuan (do ben = 200psi)

# bai 11: sigma chua biet, co mau < 30, chua co gt phan phoi chuan => kiem tra tinh chuan de dung t.test, 
# neu ko thi dung wilcox.test
x = c(1.83, 1.73, 1.76,1.85,1.79,1.93,1.84,1.88,1.81,1.83,1.80,1.88,1.68,1.79,1.82,1.77,1.92,1.79,1.94,1.96)

var(x)*19/

(7.6)**2
(7.6)*(7.6)

chi = 0.005252632 * 19 / 0.076 
qchisq(0.05, 19, lower.tail = FALSE)

shapiro.test(x) # => X co pp chuan => dung t.test
t.test(x, mu = 1.78, conf.level = 0.95)
# p-value = 0.006 < alpha = 0.05 => bac bo H0: chieu cao tb la 1.78

wilcox.test(x, mu = 1.78, conf.level = 0.95)

# bai 14: sigma biet, biet sample mean => dung zsum
zsum.test(mean.x = 7.2, sigma.x = 1.2, n.x = 16, alternative = "l", mu = 7.6, conf.level = 0.99)

# a: p-value = 0.1824 > 0.05 => tam thoi chap nhan H0: trong luong TB = 7.6

# bai 15: biet sigma, biet mean(x)
zsum.test(mean.x = 105, sigma.x = 5, n.x = 20, alternative = "g", mu = 100, conf.level = 0.95)

# bai 16: biet sigma, biet mean(x), biet
zsum.test(mean.x = 2.95, sigma.x = 1, alternative = "t", mu = 3, n.x = 2500, conf.level = 0.95)
# p-value = 0.012 < 0.05 => bac bo H0: so rang sau = 3, chap nhan H1: so rang sau khac 3

# tiep tuc kiem dinh xem so rang sau tang hay giam so voi 3
zsum.test(mean.x = 2.95, sigma.x = 1, alternative = "l", mu = 3, n.x = 2500, conf.level = 0.95)
# p-value = 0.006 < alpha = 0.05 => bac bo H0: so rang sau lon hon 3, chap nhan H1: so rang sau < 3 
# dung nhu quang cao, lam giam so rang sau

# bai 17: ko biet sigma, co mau < 30, ko biet pp chuan => ?????

# bai 18: ko biet sigma, n < 30, co pp chuan => t.test
x = c(237, 242, 232, 242, 248, 230, 244, 243, 254, 262, 234, 220, 225, 236, 232, 218, 228, 240)
t.test(x = x, alternative = "l", mu = 240, conf.level = 0.95 )
# p-value = 0.1418 > alpha = 0.05 => chap nhan H0: thoi gian TB >= 240

# bai 19, ko biet sigma, n > 30 => z.test, khi n > 30, phuong sai mau = phuong sai tong the
zsum.test(mean.x = 0.162, sigma.x = 0.04, n.x = 40, mu = 0.15, alternative = "g", conf.level = 0.99)

# bai 20, ko biet sigma, n < 30, co pp chuan => t.test

# bai 21
x = c( rep(19, 10), rep(20, 59), rep(21,20), rep(22,6), rep(23,5) )
z.test(x = x, alternative = "t", mu = 100, sigma.x = sd(x), conf.level = 0.99)

var(x)

# bai 22
nhomI = c(5.5,6.0,7.0,6.0,7.5,6.0,7.5,5.5,7.0,6.5,8.5)
shapiro.test(nhomI) # pp chuan

nhomII = c(6.5,6.0,8.5,7.0,6.5,8.0,7.5,6.5,7.5,6.0,7.0)
shapiro.test(nhomII) # pp chuan

t.test(x = nhomI, y = nhomII, alternative = "t", paired = F, var.equal = F, conf.level = 0.95)
# p-value = 0.8274 > alpha = 0.05, chap nhan Ho: 2 pp 

wilcox.test(x = nhomI, y = nhomII, alternative = "t", paired = F, conf.level = 0.95)

# bai 24, ko biet psai, n < 30, co pp chuan, gt psai = nhau => t.test(..., var.equal = T, ...)
x1 = c(3250, 3268, 4302, 3184, 3266, 3297, 3332, 3502, 3064, 3116)
x2 = c(3094, 3106, 3004, 3066, 2984, 3124, 3316, 3212, 3380, 3018)
t.test(x = x1, y = x2, alternative = "t", paired = F, var.equal = T, conf.level = 0.95)

# bai 25, ko biet psai, n < 30, ko co pp chuan, gt psai khac nhau => wilcox.test

# bai 26
zsum.test(mean.x = 3.1, sigma.x = 1.07, n.x = 55, mean.y = 3.3, sigma.y = 1.01, n.y = 48, alternative = "t", conf.level = 0.95)

# bai 27, ko biet psai, n < 30, pp chuan, mau phu thuoc =>t.test( ..., paired = T, ... )
x1 = c(6.1, 7.0, 8.2, 7.6, 6.5, 8.4, 6.9, 6.7, 7.4, 5.8, 6.0, 7.1)
x2 = c(5.2, 7.9, 3.9, 4.7, 5.3, 5.4, 4.2, 6.1, 3.8, 6.3, 4.1, 5.3)

t.test(x = x1, y = x2, alternative = "t", paired = T, conf.level = 0.95)

# bai 28
sang = c(43, 51, 37, 24, 47, 44, 50, 55, 46)
chieu= c(41, 49, 44, 32, 46, 42, 47, 51, 49)

t.test(x = sang, y = chieu, alternative = "t", paired = T, conf.level = 0.95)

# bai 29
prop.test(x = c(30, 40), n = c(110, 150), conf.level = 0.95)
30/110
40/150

# bai 30
prop.test(x = c(0.59*375, 0.7*481), n = c(375, 481), alternative = "g", conf.level = 0.95)
# ti le nam gioi thap hon nu gioi

# bai 31, p sai chua biet, n < 30, pp chuan, mau phu thuoc => t.test( ..., paired = T, ...)
x = c(6.2, 5.8, 5.7, 6.3, 5.9, 6.1, 6.2, 5.7)
y = c(6.3, 5.7, 5.9, 6.4, 5.8, 6.2, 6.3, 5.5)
var.test(x, y, alternative = "t")

# bai 32
city_1 = c(1.18, 1.07, 1.13, 1.15, 1.14, 1.13, 1.14, 1.13, 1.03, 1.09)
city_2 = c(1.08, 1.05, 1.19, 1.17, 1.21, 1.12, 1.14, 1.14, 1.13, 1.11)

var.test(city_1, city_2, conf.level = 0.99)

#     ==================== Analysis of Variance One factor =========================     #
loai_I = c(86.92, 88, 77, 84)
loai_II = c(92,91,90,81,93)
loai_III = c(75,80,83,79)

san_luong = c(86.92, 88, 77, 84, 92,91,90,81,93, 75,80,83,79)

ten = c("loai_I","loai_I","loai_I","loai_I","loai_II","loai_II","loai_II","loai_II","loai_II","loai_III","loai_III","loai_III","loai_III")
typeof(ten = as.factor(ten))
my_data = cbind(ten, san_luong)
my_data = data.frame(ten, san_luong)

g = as.factor(ten)

res.aov = aov(san_luong ~ ten)
res.aov = aov(san_luong ~ ten, data = my_data)


summary(res.aov)

TukeyHSD(res.aov)

plot(res.aov, 2)

# test homogeneous variances
library(car)
leveneTest(san_luong ~ ten, my_data)

# test normality of residuals
aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals)

# non parametric Kruskal test
kruskal.aov = kruskal.test(san_luong ~ ten, data = my_data)
summary(kruskal.aov)

pairwise.wilcox.test(san_luong, ten, p.adjust.method = "BH")

wilcox.test(loai_II, loai_III)

#     ==================== Analysis of Variance Two factor =========================     #
Mau= c(5.8, 6.2, 5.4, 6.0, 5.2, 5.3, 5.4, 5.6, 6.2, 5.7, 5.5, 6.1, 6.0, 5.2,
       6.4, 5.5, 5.0, 5.6, 6.2, 6.1, 5.3, 6.0, 6.6, 6.1, 5.8, 5.9, 6.0, 5.9, 6.0, 6.7,
       6.5, 6.3, 6.1, 6.8, 6.4, 6.8, 6.6, 6.4, 6.2, 7.1, 7.0, 7.2, 6.2, 5.8, 6.5, 6.2,
       6.4, 5.7, 6.1, 6.8, 7.1, 6.5, 7.1, 7.2, 6.7, 7.0, 7.6, 7.7, 7.8, 6.8, 7.3, 7.1,
       7.2)
length(Mau)

PhanNhomA = rep(c(1, 2, 3), c(21, 21, 21))
PhanNhomA = as.factor(PhanNhomA)

PhanNhomB = rep(c(1, 2, 3), each = 7, length = 63)
PhanNhomB = as.factor(PhanNhomB)

res.aov = aov(Mau ~ PhanNhomA + PhanNhomB + PhanNhomA*PhanNhomB)
summary(res.aov)
TukeyHSD(res.aov, which = "PhanNhomA")

# Bai 1
nang_suat = c(86.92, 88, 77, 84, 92, 91, 81, 93, 75, 80,83,79)
length(nang_suat)

phan_bon = rep( c(1,2,3), c(4,4,4) )
phan_bon = as.factor(phan_bon)
length(phan_bon)

giong_lua = rep( c("A", "B", "C", "D"), 3 )
giong_lua = as.factor(giong_lua)
length(giong_lua)

my_data = data.frame(phan_bon, giong_lua, nang_suat)

kruskal.test(nang_suat ~ phan_bon, data = my_data)
kruskal.test(nang_suat ~ giong_lua, data = my_data)

# bai 2
luong_sua = c(4.05,4.01,4.02,4.04,3.99,4.02,4.01,3.99,4,4,3.97,3.98,3.97,3.95,4,4,4,4.02,3.99,4.01)
may_sua = c( rep(1,4), rep(2,6), rep(3,6), rep(4,4) )
may_sua = as.factor(may_sua)
my_data = data.frame(may_sua, luong_sua)

res = aov(luong_sua ~ may_sua, data = my_data)
summary(res)
TukeyHSD(res)

res2 = kruskal.test(luong_sua ~ may_sua, data = my_data)

mayI = c(4.05,4.01,4.02,4.04)
mayII = c(3.99,4.02,4.01,3.99,4,4)

pairwise.wilcox.test(luong_sua, may_sua, p.adjust.method = "BH")
wilcox.test(mayI, mayII)

# bai 3
quang_duong = c(124.1, 131.5, 127, 126.4, 130.6, 128.4, 127.2, 132.7, 125.6)
loai_xang = c( rep(1,3), rep(2,3), rep(3,3) )
loai_xang = as.factor(loai_xang)

chat_pg = rep(c("A", "B", "C"), 3)
chat_pg = as.factor(chat_pg)

my_data = data.frame(loai_xang, chat_pg, quang_duong)

kruskal.test(quang_duong ~ loai_xang, data = my_data)
kruskal.test(quang_duong ~ chat_pg, data = my_data)

# bai 5
my_data = c(18,16,23,20,23)
res = chisq.test(my_data, p = c(0.2,0.2,0.2,0.2,0.2))
res


