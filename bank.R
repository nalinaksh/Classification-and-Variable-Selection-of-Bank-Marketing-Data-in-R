set.seed(1);
bank = read.csv('/Users/nalinakshgaur/Downloads/bank/bank-full.csv', header = TRUE, sep = ';');
attach(bank);
glm.fit = glm(y~., bank, family = binomial);
glm.probs = predict(glm.fit, type = 'response');
glm.pred = rep('no',nrow(bank));
glm.pred[glm.probs > 0.5] = 'yes';
print(table(glm.pred, y));


train = sample(nrow(bank), 40000);
test = (-train);
test.X = bank[test,];
test.Y = y[test];
glm.fit = glm(y~., bank, family = binomial, subset = train);
glm.probs = predict(glm.fit, test.X, type = 'response');
glm.pred = rep('no',nrow(test.X));
glm.pred[glm.probs > 0.5] = 'yes';
print(table(glm.pred, test.Y));

k=10;
folds = sample(1:k, nrow(bank), rep = TRUE);
for(i in 1:k) {
	test.X = bank[folds == i,];
	test.Y = y[folds == i];
	glm.fit = glm(y~., bank[folds!=i,], family = binomial);
	glm.probs = predict(glm.fit, test.X, type = 'response');
	glm.pred = rep('no',nrow(test.X));
	glm.pred[glm.probs > 0.5] = 'yes';
	print(table(glm.pred, test.Y));
}

library(glmnet);
x = model.matrix(y~., bank)[,-1];
y = bank$y;
resp = rep(0,nrow(bank));
resp[y == 'yes'] = 1;
grid = 10^seq(10,-2,length=100);
train = sample(nrow(bank), 40000);
test = (-train);
test.X = bank[test,];
test.Y = resp[test];
lasso.mod = glmnet(x[train,], resp[train], alpha = 1, lambda = grid);
plot(lasso.mod);

cv.out = cv.glmnet(x[train,], resp[train], alpha = 1);
plot(cv.out);
bestlam = cv.out$lambda.min;

out = glmnet(x,resp,alpha=1,lambda=grid);
lasso.coef = predict(out, type='coefficients', s=bestlam)[1:43,];
print(lasso.coef);