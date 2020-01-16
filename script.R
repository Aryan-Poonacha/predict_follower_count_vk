#cleaning the dataset, adding required columns
vk[vk$sex == 2, ]$sex <- 0
vk$hasComments <- 0
vk[vk$boolComments == "True", ]$hasComments <- 1
vk[vk$boolComments == "False", ]$hasComments <- 0
#applying transformation
vk$countOwnerPostsTransformed <- vk$countOwnerPosts^2
#response variable distributions
hist_followers <- ggplot(vk, aes(x = vk$countFollowers, y=
..density..))+ geom_histogram(bins = 150, color="black",
fill="green")+
 geom_density(alpha=.2, fill="#FF6666")
hist_followers<-hist_followers +
geom_vline(aes(xintercept=median(vk$countFollowers)),
 color="blue",linetype="dashed", size=1)
hist_followers<-hist_followers +
geom_vline(aes(xintercept=mean(vk$countFollowers)),
color="red",linetype="dashed", size=1)
hist_followers<-hist_followers + xlab("Number Of Followers")+
ylab("Frequency")
hist_followers <- hist_followers + ggtitle("Distribution Of
Number Of Followers Of VK Accounts")
hist_followers
boxplot_length <-boxplot(vk$countFollowers,
 main = "Distribution Of Number Of
Followers",
 names = "Number Of Followers",
 col = "green",
 border = "black",
 xlab="Number Of Followers",
 horizontal = TRUE,
 outcex = 0.6
)
#predictor variable distributions
hist_friends <- ggplot(vk, aes(x = vk$countFriends, y=
..density..))+ geom_histogram(bins = 21, color="black",
fill="grey")+
 geom_density(alpha=.2, fill="#FF6666")
hist_friends<-hist_friends + xlab("Number Of Friends")+
ylab("Frequency")
hist_friends <- hist_friends + ggtitle("Distribution Of Number
Of Friends Of VK Accounts")
hist_friends
hist_OwnerPosts <- ggplot(vk, aes(x = vk$countOwnerPosts, y=
..density..))+ geom_histogram(bins = 21, color="black",
fill="grey")+
 geom_density(alpha=.2, fill="#FF6666")
hist_OwnerPosts<-hist_OwnerPosts + xlab("Number Of Owner
Posts")+ ylab("Frequency")
hist_OwnerPosts <- hist_OwnerPosts + ggtitle("Distribution Of
Number Of Owner Posts Of VK Accounts")
hist_OwnerPosts
hist_OwnerPostsTransformed <- ggplot(vk, aes(x =
vk$countOwnerPostsTransformed, y= ..density..))+
geom_histogram(bins = 21, color="black", fill="grey")+
 geom_density(alpha=.2, fill="#FF6666")
hist_OwnerPostsTransformed<-hist_OwnerPostsTransformed +
xlab("Number Of Owner Posts Transformed")+ ylab("Frequency")
hist_OwnerPostsTransformed <- hist_OwnerPostsTransformed +
ggtitle("Distribution Of Number Of Owner Posts Transformed Of VK
Accounts")
hist_OwnerPostsTransformed
hist_OwnerReposts <- ggplot(vk, aes(x = vk$countOwnerReposts, y=
..density..))+ geom_histogram(bins = 21, color="black",
fill="grey")+
 geom_density(alpha=.2, fill="#FF6666")
hist_OwnerReposts<-hist_OwnerReposts + xlab("Number Of Owner
Reposts")+ ylab("Frequency")
hist_OwnerReposts <- hist_OwnerReposts + ggtitle("Distribution
Of Number Of Owner Reposts Of VK Accounts")
hist_OwnerReposts
hist_Photos <- ggplot(vk, aes(x = vk$countPhotos, y=
..density..))+ geom_histogram(bins = 21, color="black",
fill="grey")+
 geom_density(alpha=.2, fill="#FF6666")
hist_Photos<-hist_Photos + xlab("Number Of Owner Photos")+
ylab("Frequency")
hist_Photos <- hist_Photos + ggtitle("Distribution Of Number Of
Owner Photos Of VK Accounts")
hist_Photos
hist_Videos <- ggplot(vk, aes(x = vk$countVideos, y=
..density..))+ geom_histogram(bins = 21, color="black",
fill="grey")+
 geom_density(alpha=.2, fill="#FF6666")
hist_Videos<-hist_Videos + xlab("Number Of Owner Videos")+
ylab("Frequency")
hist_Videos<- hist_Videos + ggtitle("Distribution Of Number Of
Owner Videos Of VK Accounts")
hist_Videos
hist_LikesPhotoes <- ggplot(vk, aes(x = vk$countLikesPhotoes, y=
..density..))+ geom_histogram(bins = 21, color="black",
fill="grey")+
 geom_density(alpha=.2, fill="#FF6666")
hist_LikesPhotoes<-hist_LikesPhotoes + xlab("Number Of Owner
Photo Likes")+ ylab("Frequency")
hist_LikesPhotoes <- hist_LikesPhotoes + ggtitle("Distribution
Of Number Of Photo Likes Of VK Accounts")
hist_LikesPhotoes
grid.arrange(hist_friends,hist_LikesPhotoes,hist_OwnerPosts,
hist_OwnerPostsTransformed,
hist_OwnerReposts,hist_Photos,hist_Videos)
#corr plot
vk_numeric <- subset(vk, select=-c(ID,boolComments))
corrs <- cor(data.frame(vk_numeric$countFollowers, vk_numeric),
use="complete.obs")
corrs <- subset(corrs, select=-c(vk_numeric.countFollowers))
corrs <- tail(corrs,-1)
corrplot <- ggcorrplot(corrs, lab = TRUE) +ggtitle("Correlation
Table")
corrplot
#regression
lmodel_vk <- lm(countFollowers ~ countFriends +
countLikesPhotoes + countOwnerPostsTransformed, data=vk)
summary(lmodel_vk)
# Creates the residual plot of this data
predicted<-predict(lmodel_vk)
resids<-residuals(lmodel_vk)
lmodel_vk.data<-data.frame(predicted,resids)
lmodel_vk_resid <- ggplot(lmodel_vk.data,
aes(x=predicted,y=resids)) +
 geom_point() +
 xlab("Predicted Number Of Followers") +
 ylab("Residuals Of Followers") +
 ggtitle("Residuals vs Predicted Number Of Followers") +
 geom_hline(yintercept = 0, color = "black")
lmodel_vk_resid
#distribution of the residuals
hist_resids <- ggplot(lmodel_vk.data,
aes(x=resids))+geom_histogram(bins = 250)+xlab("Residuals Of
Follower Count") +ylab("Frequency") + ggtitle("Distribution Of
Predicted Follower Count Residuals")
hist_resids
#partial regression plots
partial_friends <- visreg(lmodel_vk, "countFriends",
gg=TRUE)+xlab("Number of Friends") +ylab("Residuals Of
Followers") +ggtitle("Partial Regression of No. of Friends vs
No. of Followers")
partial_LikesPhotoes <- visreg(lmodel_vk, "countLikesPhotoes",
gg=TRUE)+xlab("Number Of Photo Likes") +ylab("Residuals Of
Followers") +ggtitle("Partial Regression of No. of Photo Likes
vs No. of Followers")
partial_OwnerPostsTransformed <- visreg(lmodel_vk,
"countOwnerPostsTransformed", gg=TRUE)+xlab("Number of Posts
Transformed") +ylab("Residuals Of Followers") +ggtitle("Partial
Regression of No. of Posts Transformed vs No. of Followers")
grid.arrange(partial_friends, partial_LikesPhotoes,
partial_OwnerPostsTransformed)
#subsetting into male and female for t-test
male <- subset(vk, vk$sex == 1)
female <- subset(vk, vk$sex == 0)
#distributions of male and female for nearly normal condition
hist_male_followers <- ggplot(male, aes(x = male$countFollowers,
y= ..density..))+ geom_histogram(bins = 100, color="black",
fill="lightblue")+
 geom_density(alpha=.2, fill="#FF6666")
hist_male_followers<-hist_male_followers + xlab("Number Of
Followers")+ ylab("Frequency")
hist_male_followers<-hist_male_followers+ ggtitle("Distribution
Of Number Of Followers Of Male VK Accounts")
hist_male_followers
hist_female_followers <- ggplot(female, aes(x =
female$countFollowers, y= ..density..))+ geom_histogram(bins =
100, color="black", fill="pink")+
 geom_density(alpha=.2, fill="#FF6666")
hist_female_followers<-hist_female_followers + xlab("Number Of
Followers")+ ylab("Frequency")
hist_female_followers<-hist_female_followers+
ggtitle("Distribution Of Number Of Followers Of Female VK
Accounts")
hist_female_followers
grid.arrange(hist_male_followers, hist_female_followers)
#conduct the t-test
t.test(male$countFollowers, female$countFollowers, alternative =
"two.sided", var.equal = FALSE)
#subsetting into comments and no comments
comments <- subset(vk, vk$hasComments == 1)
nocomments <- subset(vk, vk$hasComments == 0)
#distributions of comments and no comments for nearly normal
condition
hist_comments_followers <- ggplot(comments, aes(x =
comments$countFollowers, y= ..density..))+ geom_histogram(bins =
100, color="black", fill="lightblue")+
 geom_density(alpha=.2, fill="#FF6666")
hist_comments_followers<-hist_comments_followers + xlab("Number
Of Followers")+ ylab("Frequency")
hist_comments_followers<-hist_comments_followers +
ggtitle("Distribution Of Number Of Followers Of VK Accounts With
Comments")
hist_comments_followers
hist_nocomments_followers <- ggplot(nocomments, aes(x =
nocomments$countFollowers, y= ..density..))+ geom_histogram(bins
= 100, color="black", fill="pink")+
 geom_density(alpha=.2, fill="#FF6666")
hist_nocomments_followers<-hist_nocomments_followers +
xlab("Number Of Followers")+ ylab("Frequency")
hist_nocomments_followers<-hist_nocomments_followers +
ggtitle("Distribution Of Number Of Followers Of VK Accounts With
No Comments")
hist_nocomments_followers
grid.arrange(hist_comments_followers, hist_nocomments_followers)
#conduct the t-test
t.test(comments$countFollowers, nocomments$countFollowers,
alternative = "greater", var.equal = FALSE)