## spam Dataset
library(kernlab)
data('spam')
tibble::as.tibble(spam)
is.factor(spam$type)
levels(spam$type)
set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]
fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)
# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)#0.34
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)#0.212
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)#0.0644
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)#0.063
library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]#0.2134
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]#0.1522
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]#0.0784
cv.glm(spam_trn, fit_over, K = 5)$delta[1]#0.108
###
set.seed(2)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]#0.2138123
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]#0.153601
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]#0.06699597
cv.glm(spam_trn, fit_over, K =  100)$delta[1]#0.09655727

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}
spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
table(spam_tst$type) / nrow(spam_tst)

#fit_caps
spam_tst_pred = ifelse(predict(fit_caps, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_caps, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
cat('accuracy:',sum(diag(conf_mat_50))/sum(conf_mat_50))
#take spam as positive event
cat('sens:',conf_mat_50[2,2]/(conf_mat_50[2,2]+conf_mat_50[1,2]))
cat('spec:',conf_mat_50[1,1]/(conf_mat_50[1,1]+conf_mat_50[2,1]))

#fit_selected
spam_tst_pred = ifelse(predict(fit_selected, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_selected, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
cat('accuracy:',sum(diag(conf_mat_50))/sum(conf_mat_50))
#take spam as positive event
cat('sens:',conf_mat_50[2,2]/(conf_mat_50[2,2]+conf_mat_50[1,2]))
cat('spec:',conf_mat_50[1,1]/(conf_mat_50[1,1]+conf_mat_50[2,1]))

#fit_additive
spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
cat('accuracy:',sum(diag(conf_mat_50))/sum(conf_mat_50))
#take spam as positive event
cat('sens:',conf_mat_50[2,2]/(conf_mat_50[2,2]+conf_mat_50[1,2]))
cat('spec:',conf_mat_50[1,1]/(conf_mat_50[1,1]+conf_mat_50[2,1]))

#fit_over
spam_tst_pred = ifelse(predict(fit_over, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_over, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
cat('accuracy:',sum(diag(conf_mat_50))/sum(conf_mat_50))
#take spam as positive event
cat('sens:',conf_mat_50[2,2]/(conf_mat_50[2,2]+conf_mat_50[1,2]))
cat('spec:',conf_mat_50[1,1]/(conf_mat_50[1,1]+conf_mat_50[2,1]))

bank <- read.csv('~/Desktop/Logistic_Regression/bank.csv')
head(bank)
set.seed(1)
bank_idx = sample(nrow(bank), 500)
bank_trn = bank[bank_idx, ]
bank_tst = bank[-bank_idx, ]
fit_additive = glm(y ~ age+job+marital+education,
                   data = bank_trn, family = binomial)
cv.glm(bank_trn, fit_additive, K = 10)$delta[1]#0.09757089
bank_tst_pred = ifelse(predict(fit_additive, bank_tst) > 0,
                       "yes",
                       "no")
bank_tst_pred = ifelse(predict(fit_additive, bank_tst, type = "response") > 0.5,
                       "yes",
                       "no")
(conf_mat_50 = make_conf_mat(predicted = bank_tst_pred, actual = bank_tst$y))


#fit_additive
fit_additive = glm(y ~ age+job+marital+education,
                   data = bank_trn, family = binomial)
cv.glm(bank_trn, fit_additive, K = 10)$delta[1]#0.09757089
bank_tst_pred = ifelse(predict(fit_additive, bank_tst) > 0,
                       "yes",
                       "no")
bank_tst_pred = ifelse(predict(fit_additive, bank_tst, type = "response") > 0.5,
                       "yes",
                       "no")
(conf_mat_50 = make_conf_mat(predicted = bank_tst_pred, actual = bank_tst$y))
#fit_all
fit_all = glm(y ~ .,
              data = bank_trn, family = binomial)
cv.glm(bank_trn, fit_all, K = 10)$delta[1]#0.09757089
bank_tst_pred = ifelse(predict(fit_all, bank_tst) > 0,
                       "yes",
                       "no")
bank_tst_pred = ifelse(predict(fit_all, bank_tst, type = "response") > 0.5,
                       "yes",
                       "no")
(conf_mat_50 = make_conf_mat(predicted = bank_tst_pred, actual = bank_tst$y))
