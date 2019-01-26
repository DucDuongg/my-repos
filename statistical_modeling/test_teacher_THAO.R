library('BSDA')

# bai 1
# X ~ N(mu, 0.08**2), sigma = 0.08**2
pcb = c(11.2, 12.4, 10.8, 11.6, 12.5, 10.1, 12.2, 10.6)

z.test(x = pcb, sigma.x = 0.08, conf.level = 0.95)

# bai 2

zsum.test(mean.x = 1.2, sigma.x = 0.2, n.x = 20, conf.level = 0.99)

# bai 3
tsum.test(mean.x = 1.2, s.x = 0.2, n.x = 20, conf.level = 0.99)

# bai 4
nhietdohoalong = c(330, 322, 345, 328.6, 331, 342, 342.4, 340, 329.7, 334, 326.5, 325.8, 337.5, 327.3, 
                   322.6, 341, 340, 333, 343.3, 331, 341, 329.5, 332.3, 340)

mean(nhietdohoalong)

t.test(x = nhietdohoalong, conf.level = 0.99)

# bai 5
NegLogLik1=function(mu,sigma) {
  -sum(dnorm(x,mean=mu,sd=sigma,log=TRUE))
}

require(stats4)

x = c(140, 136, 150, 144, 148, 152, 138, 141)

MLE = mle(minuslogl=NegLogLik1, start=list(mu=mean(x),sigma=sd(x)))
summary(MLE)

var(x)

df <- length(x) - 1
varX <- var(x)
lower = varX * df / qchisq(0.01/2, df, lower.tail = FALSE)
upper = varX * df / qchisq((1 - 0.01)/2, df, lower.tail = FALSE)
c(lower = lower, variance = varX, upper = upper)

# bai 7
p0 = 5016/10000
p0

prop.test(x = 5016, n = 10000, conf.level = 0.99)

# bai 8
prop.test(x = 16, n = 100, conf.level = 0.95)

# bai 9
x = c(8.18, 8.17, 8.16, 8.15, 8.17, 8.21, 8.22, 8.16, 8.19, 8.18)

z.test(x = x, sigma.x = 0.02, mu = 8.2, alternative = "two.sided", conf.level = 0.95)

# bai 10
x = c(210, 198, 195, 202, 197.5, 196, 199, 195.5)
z.test(x, mu = 200, alternative = "less", sigma.x = 5, conf.level = 0.95)

# bai 11
x = c(1.83, 1.73, 1.76, 1.85, 1.81, 1.83, 1.8, 1.88, 1.68, 1.79, 1.79, 1.93, 1.84, 1.88, 1.82, 1.77, 1.92, 1.79, 1.94, 1.96)
z.test(x, mu = 1.78, sigma.x = 7.6, conf.level = 0.95)

# bai 14
zsum.test(mean.x = 7.2, sigma.x = 1.2, n.x = 16, mu = 7.6, alternative = "less", conf.level = 0.99)

# bai 15
zsum.test(mean.x = 105, sigma.x = 5, n.x = 20, mu = 100, alternative = "g")

# bai 16
zsum.test(mean.x = 2.95, sigma.x = 1, n.x = 2500, mu = 3, alternative = "l", conf.level = 0.95)
# p-value = 0.00621 < alpha = 0.05 => chap nhan H1: loai thuoc danh rang nay lam giam so rang sau

# bai 17
tsum.test(mean.x = 19.7, s.x = 1.3, n.x = 25, alternative = "two.sided", mu = 20, conf.level = 0.95)
# p-value = 0.2599 > alpha = 0.95 => do not reject H0: mean is equal 20

# bai 18
x = c(237, 242, 232, 242, 248, 230, 244, 243, 254, 262, 234, 220, 225, 236, 232, 218, 228, 240)
t.test(x, alternative = "less", mu = 240, conf.level = 0.95)
# p-value = 0.1418 > alpha = 0.05 => chua du co so de bac bo H0

# bai 19
tsum.test( mean.x = 0.162, s.x = 0.04, n.x = 40, alternative = "greater", mu = 0.15, conf.level = 0.99 )
# p-value = 0.0326 > alpha = 0.01 => chua du co so de bac bo H0: ti le luu huynh <= 0.15

# bai 20
x = c(30.1, 32.7, 22.5, 27.5, 27.7, 29.8, 28.9, 31.4, 31.2, 24.3, 26.4, 22.8, 29.1, 33.4, 32.5, 21.7)
t.test(x, mu = 30, alternative = "less")
# p-value = 0.04 < alpha = 0.05 => reject H0: kha nang chiu luc cua tam nhua >= 30 psi, accept H1: kha nang chiu luc < 30 psi

# bai 21
x = c( rep(19,10), rep(20, 59), rep(21, 20), rep(22, 6), rep(23, 5) )
t.test(x, mu = 20, conf.level = 0.99)
# p-value = 0.0001287 < alpha = 0.01 => bac bo H0: trong luong san pham trung binh la 20kg, tam ket luan may moc hoat dong
# khong binh thuong (H1: trong luong san pham trung binh khac 20kg)

# bai 22
vitaminC = c(5.5, 6.0, 7.0, 6.0, 7.5, 6.0, 7.5, 5.5, 7.0, 6.5, 8.5)
without_vitaminC = c(6.5, 6.0, 8.5, 7.0, 6.5, 8.0, 7.5, 6.5, 7.5, 6.0, 7.0)

# test phan phoi chuan cua 2 bien
qqnorm(vitaminC)
qqline(vitaminC)

qqnorm(without_vitaminC)
qqline(without_vitaminC)
# OK

# 2 bien co phan phoi chuan, 2 mau doc lap (paired = False), phuong sai chua biet, n < 30 => t.test
t.test(vitaminC, without_vitaminC, alternative = "l", paired = F, var.equal = F, conf.level = 0.95)
# p-value = 0.1726 > alpha = 0.05 => chap nhan H0: 2 phuong phap hieu qua nhu nhau 
# bac bo H1: phuong phap vitamin C giam thoi gian cam lanh (gia tri trung)

# bai 24
loai_I = c(3250, 3268, 4302, 3184, 3266, 3297, 3332, 3502, 3064, 3116)
loai_II = c(3094, 3106, 3004, 3066, 2984, 3124, 3316, 3212, 3380, 3018)

# phan phoi chuan, 2 mau doc lap, phuong sai bang nhau (var.equal = T) => t.test
t.test(loai_I, loai_II, var.equal = T)
# p-value = 0.07 > alpha = 0.05 => chap nhan H0: kha nang chiu luc cua 2 loai be tong la nhu nhau
# chua du co so de ket luan kha nang chiu luc cua 2 loai be tong la khac nhau

# bai 25
# 2 mau doc lap (paired = false), phuong sai khac nhau, KHONG CO gia thiet phan phoi chuan => wilcox.test

# bai 26

# bai 27
thuoc_that = c(6.1, 7.0, 8.2, 7.6, 6.5, 8.4, 6.9, 6.7, 7.4, 5.8, 6.0, 7.1)
thuoc_gia  = c(5.2, 7.9, 3.9, 4.7, 5.3, 5.4, 4.2, 6.1, 3.8, 6.3, 4.1, 5.3)

# mau phu thuoc (cung benh nhan), paired = T, co phan phoi chuan, n nho chua biet phuong sai tong the => t.test
t.test(thuoc_that, thuoc_gia, paired = T)
# p-value = 0.002564 < alpha = 0.05 => bac bo H0: tac dung 2 thuoc nhu nhau

# bai 28
# mau phu thuoc, n nho, phan phoi chuan, chua biet phuong sai tong the => t.test, paired = T
sang = c(43, 51, 37, 24, 47, 44, 50, 55, 46)
chieu= c(41, 49, 44, 32, 46, 42, 47, 51, 49)

t.test(x = sang, y = chieu, paired = T)
# p-value 0.772 > alpha = 0.05 => accept H0: luong khach sang - chieu la nhu nhau

# bai 29
prop.test( x = c(30, 40), n = c(110, 150))

# bai 30
prop.test( x = c(221.25, 336.7), n = c(375, 481), alternative = "less" )
# p-value = 0.00052 < alpha = 0.05 => chap nhan H1: ti le phu nu co y kien cao hon nam gioi

# bai 31
x = c(6.2, 5.8, 5.7, 6.3, 5.9, 6.1, 6.2, 5.7)
y = c(6.3, 5.7, 5.9, 6.4, 5.8, 6.2, 6.3, 5.5)

var.test(x = x, y = y)
# p-value = 0.4237 > alpha = 0.05 => accept H0: phuong sai 2 phuong phap la nhu nhau

# bai 32
city_1 = c(1.18, 1.07, 1.13, 1.15, 1.14, 1.13, 1.14, 1.13, 1.03, 1.09)
city_2 = c(1.08, 1.05, 1.19, 1.17, 1.21, 1.12, 1.14, 1.14, 1.13, 1.11)

var.test(city_1, city_2, conf.level = 0.99)
# p-value = 0.7618 > alpha = 0.01 => accept H0: do lech gia xang giua 2 thanh pho la nhu nhau

