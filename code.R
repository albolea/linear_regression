library(Lahman)
library(tidyverse)
library(dslabs)
library(ggthemes)
ds_theme_set()


Teams %>% filter(yearID %in% 1961:2001) %>% 
          mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
          ggplot(aes(HR_per_game,R_per_game)) + geom_point(alpha=0.5) + theme_economist()

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(SB_per_game = SB/G, R_per_game = R/G) %>%
  ggplot(aes(SB_per_game,R_per_game)) + geom_point(alpha=0.5) + theme_economist()

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game,R_per_game)) + geom_point(alpha=0.5) + theme_economist()

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) + theme_economist()

?Teams

library(HistData)
data("GaltonFamilies")
GaltonFamilies %>% head()

galton_heights <- GaltonFamilies %>% filter(childNum == 1 & gender == "male") %>% 
  select(father,childHeight) %>% rename(son = childHeight)

galton_heights %>% summarise(mean(father), sd(father),mean(son),sd(son))

galton_heights %>% ggplot(aes(father,son)) + geom_point(alpha=0.5) + theme_economist()

galton_heights %>% summarise(cor(father,son))

set.seed(0)

B <- 1000
N <-25

R<- replicate(B, {
    sample_n(galton_heights,N,replace = T) %>% summarise(r=cor(father,son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color="black")+theme_economist()
mean(R)
sd(R)

# if Normal dist
nd_intercept <- mean(R)
nd_slope <- sqrt((1-mean(R)^2)/(N-2))

data.frame(R) %>% 
  ggplot(aes(sample=R)) + 
  stat_qq() + 
  geom_abline(intercept = nd_intercept, 
              slope = nd_slope) + theme_economist()

conditional_avg <- galton_heights %>% filter(round(father)==72) %>% summarise(avg=mean(son)) %>% .$avg
conditional_avg

galton_heights %>% mutate(father_strdata = factor(round(father))) %>% ggplot(aes(father_strdata,son)) +
  geom_boxplot()+geom_point()+theme_economist()

galton_heights %>% mutate(father = factor(round(father)))%>% group_by(father) %>% 
  summarise(son_conditional_avg = mean(son)) %>%  ggplot(aes(father,son_conditional_avg)) +
  geom_point()+theme_economist()

r<- galton_heights %>% summarise(r=cor(father,son)) %>% .$r
galton_heights %>% mutate(father = round(father)) %>% group_by(father) %>% 
  summarise(son = mean(son)) %>% mutate(z_father = scale(father),z_son = scale(son)) %>%
  ggplot(aes(z_father,z_son)) + geom_point() +geom_abline(intercept = 0,slope = r)+theme_economist()


mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)

m <-  r * s_y / s_x
b <- mu_y - m*mu_x

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m ) 

galton_heights %>% 
  ggplot(aes(scale(father), scale(son))) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r) 

galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father) 

mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = b_1, slope = m_1, col = "blue") +
  geom_abline(intercept = -b_2/m_2, slope = 1/m_2, col = "red") 


Teams %>% filter(yearID %in% 1961:2001) %>% mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarise(cor(BB,HR),cor(Singles,HR), cor(BB,Singles))

dat <- Teams %>% filter(yearID %in% 1961:2001) %>% mutate(HR_strdata = round(HR/G,1),
                                                          BB_per_game = BB/G,
                                                          R_per_game = R/G) %>%
  filter(HR_strdata>=0.4 & HR_strdata<=1.2)

dat%>% ggplot(aes(BB_per_game,R_per_game)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strdata)

dat%>% group_by(HR_strdata) %>% summarise(slope = cor(BB_per_game,R_per_game)*sd(R_per_game)/sd(BB_per_game))

dat <- Teams %>% filter(yearID %in% 1961:2001) %>% mutate(BB_strdata = round(BB/G,1),
                                                          HR_per_game = HR/G,
                                                          R_per_game = R/G) %>%
  filter(BB_strdata>=2.8 & BB_strdata<=3.9)

dat%>% ggplot(aes(HR_per_game,R_per_game)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") +
  facet_wrap(~BB_strdata)

dat%>% group_by(BB_strdata) %>% summarise(slope = cor(HR_per_game,R_per_game)*sd(R_per_game)/sd(HR_per_game))

rss <- function(beta0,beta1,data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 <- seq(0,1,len=nrow(galton_heights))
results<- data.frame(beta1 = beta1,
                     rss = sapply(beta1,rss,beta0=36))
results %>% ggplot(aes(beta1,rss)) + geom_line(col=2)

fit <- lm(son ~ father, data = galton_heights)
fit
summary(fit)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>% mutate(HR_per_game = HR/G,
                                                          BB_per_game = BB/G,
                                                          R_per_game = R/G)
lm(dat$R_per_game ~ dat$BB_per_game+dat$HR_per_game,data = dat)


B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

lse %>% summarize(cor(beta_0, beta_1))

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,]) 

galton_heights %>% ggplot(aes(son,father)) + geom_smooth(method = "lm") + geom_point()
galton_heights %>% mutate(Y_hat = predict(lm(son~father,data=.))) %>% 
  ggplot(aes(father,Y_hat)) + geom_line()
fit <- galton_heights %>% lm(son~father,data=.)
Y_hat <- predict(fit,se.fit=T)
names(Y_hat)

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2) 
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

dat %>% group_by(HR) %>% lm(R~BB,data =.) %>% .$coef

dat %>% group_by(HR) %>% head()

dat %>% group_by(HR) %>% do(fit=lm(R~BB,data =.))

get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))

library(broom)
fit <- lm(R~BB,data=dat)
tidy(fit)
tidy(fit,conf.int = T)
dat %>% group_by(HR) %>% do(tidy(lm(R~BB,data=.),conf.int=T)) %>% filter(term=="BB") %>%
  select(HR,estimate,conf.low,conf.high)

dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G,
        singles = (H -X2B - X3B-HR) /G,
        doubles = X2B/G,
        triples = X3B/G,
        HR = HR/G,
        R = R/G) %>%
  lm(R ~ BB+singles+doubles+triples+HR,data=.)
coefs <- tidy(fit)
coefs

Teams %>% filter(yearID %in% 2002) %>% 
          mutate(BB = BB/G,
                 singles = (H -X2B - X3B-HR) /G,
                 doubles = X2B/G,
                 triples = X3B/G,
                 HR = HR/G,
                 R = R/G) %>%
          mutate(R_hat = predict(fit,newdata = .)) %>%
          ggplot(aes(R_hat, R, label = teamID)) + 
          geom_point() +
          geom_text(nudge_x=0.1, cex = 2) + 
          geom_abline()

pa_per_game <- Batting %>% 
                filter(yearID == 2002) %>%
                group_by(teamID) %>%
                summarise(pa_per_game = sum(AB+BB)/max(G)) %>% .$pa_per_game %>% mean

players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))  

players %>% ggplot(aes(R_hat)) + 
  geom_histogram(binwidth = 0.5, color = "black")

players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

players <- Fielding %>% filter(yearID == 2002) %>%
  filter(!POS %in% c("OF","P")) %>%
  group_by(playerID) %>%
  top_n(1, G) %>%
  filter(row_number(G) == 1) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by="playerID")
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

players %>% filter(debut < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

tidy(fit)
a<-c(2,4,1,0,1)

fit %>% coef
f <- fit$coef
a<-c(1,2,4,1,0,1)
a*f
sum(a*f)
b<-c(1,1,6,2,1,0)
sum(b*f)

library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= 1997 & debut > 1988)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

Batting %>% 
  filter(yearID %in% 1990:2001) %>% 
  group_by(playerID, yearID) %>%
  filter(BB + AB >= 300) %>%
  mutate(PA = BB + AB, 
         singles = (H-X2B-X3B-HR),
         OPS = BB / PA + 
           (singles + 2*X2B + 3*X3B + 4*HR)/AB,
         G = PA/pa_per_game, 
         BB = BB/G,
         singles = singles/G,
         doubles = X2B/G, 
         triples = X3B/G,
         HR = HR/G) %>%
  ungroup() %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ungroup %>%
  ggplot(aes(OPS, R_hat)) + 
  geom_point()

playerInfo <- Fielding %>% 
  group_by(playerID) %>% 
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>% 
  left_join(Master, by="playerID") %>% 
  select(playerID, nameFirst, nameLast, POS)
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>% 
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>% 
  mutate(AVG = H/AB) %>% 
  filter(POS != "P")
ROY <- ROY %>%  
  filter(yearID == rookie_year | yearID == rookie_year+1) %>% 
  group_by(playerID) %>% 
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>% 
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG) 
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

mean(ROY$sophomore - ROY$rookie < 0)
mean(round(ROY$sophomore,2) - round(ROY$rookie,2) < 0)


two_years <- Batting %>% 
  filter(yearID %in% 2013:2014) %>% 
  group_by(playerID, yearID) %>%  
  filter(sum(AB) >= 130) %>% 
  summarize(AVG = sum(H)/sum(AB)) %>% 
  ungroup %>% 
  spread(yearID, AVG) %>% 
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>% 
  filter(POS!="P") %>% 
  select(-POS) %>%
  arrange(desc(`2013`)) %>% 
  select(-playerID)
two_years


library(Lahman)
library(tidyverse)
library(dslabs)
library(ggthemes)
data(admissions)
admissions
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted*applicants)/sum(applicants),
            percent_women_applicants = sum(applicants*(gender=="women")/sum(applicants))*100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()+geom_point()+theme_economist()

admissions %>% 
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()+theme_economist()
