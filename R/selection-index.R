### Building Genetic parameters matrices, economic weights
### and calculating selection index coefficients

## Variance-covariance matrix for selection criteria (Gss)

# Correlation matrix
Css <- matrix(c(1, 0.05, 0.29, -0.25, -0.35,
                0.05, 1, 0.15, 0.05, 0.05,
                0.29, 0.15, 1, -0.05, -0.1,
                -0.25, 0.05, -0.05, 1, 0.48,
                -0.35, 0.05, -0.1, 0.48,1),
              nrow=5, ncol=5, byrow = TRUE)

# Phenotypic variances, heritabilities and additive variances
pss <- c(1100, 3.8, 4.7, 552, 2.08)
h2ss <- c(0.18, 0.25, 0.35, 0.65, 0.55)
ass <- pss * h2ss

Sss <- diag(sqrt(ass))


# Final covariance matrix
Gss <- Sss %*% Css %*% Sss
colnames(Gss) <- rownames(Gss) <- c('DBH08', 'STR08', 'BR08', 'DEN08', 'MoE08')

# Removing intermediate values
rm(pss, h2ss, ass, Css)

## Variance-covariance matrix between selection criteria
## and objective traits (Gso)

# Correlation matrix
Cso <- matrix(c(0.7, 0.1, 0.45, -0.3,
                0.14, -0.7, 0.05, -0.1,
                0.15, 0.05, -0.7, -0.11,
                -0.19, 0.09, -0.11, 0.45,
                -0.2, -0.25, -0.14, 0.7),
              nrow=5, ncol=4, byrow=TRUE)

# Phenotypic variances, heritabilities and additive variances
poo <- c(0.3, 0.49, 95, 1.7)
h2oo <- c(0.25, 0.14, 0.15, 0.4)
aoo <- poo * h2oo

Soo <- diag(sqrt(aoo))

# Final covariance matrix
Gso <- Sss %*% Cso %*% Soo

rownames(Gso) <- c('DBH', 'STR', 'BR9', 'DEN', 'MoE')
colnames(Gso) <- c('VOL', 'SWE', 'BIX', 'MoE')

# Removing intermediate values
rm(poo, h2oo, aoo, Sss, Soo, Cso)


### Economic weights from IO009, page 6
wDS <- c(4247, -499, -55, 700)

### Index coefficients for Direct Sawlog regime
bDS <- solve(Gss) %*% Gso %*% wDS
rownames(bDS) <- c('DBH', 'STR', 'BR9', 'DEN', 'MoE')
