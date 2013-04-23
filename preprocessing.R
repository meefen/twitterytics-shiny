## Preprocessing
#  To speed up the application, this script is trying to do 
#  those weight-lefting tasks in advance, and save processed
#  data in advance.

source("utilities.R", local=TRUE)
source("get_tweets.R")
source("munge_tweets.R")
source("semantic_analysis.R")
source("social_analysis.R")

# get tweets
df <- GetTweetsFromGoogleDrive("0Aup6zwZoYbZ1dFdMamNnMlNDdGFsSFB3QmhOUDd4dVE")
# remove duplicates
df <- df[!duplicated(df$id_str), ]
# # only select tweets during conference
# start <- strptime("10/04/2013 00:00:01", "%d/%m/%Y %H:%M:%S")
# df <- df[(df$created_at > start), ]

df <- PreprocessTweets(df)
save(df, file="data/aera13.Rda")
write.csv(df, file="data/aera13.csv")

# wordcloud
corpus <- ConstructCorpus(df$text, removeTags=TRUE, removeUsers=TRUE)
save(corpus, file="data/corpus.Rda")
# png('data/cloud.png')
# cloud <- MakeWordCloud(corpus)
# dev.off()

# options(DropboxKey = "a0d223msqt3gpba")
# options(DropboxSecret = "bqa4tzf8qidx9lt")
# library(rDrop)
# dropbox_credentials <- dropbox_auth("a0d223msqt3gpba", "bqa4tzf8qidx9lt")
# save(dropbox_credentials, file="~/my_dropbox_credentials.rdata")
# load('/path/to/my_dropbox_credentials.rdata')
# dropbox_acc_info(dropbox_credentials)

# sentiments
sentiments <- ScoreSentiment(df$text, .progress="text")
sentiments$from_user <- df$from_user
sentiments <- sentiments[with(sentiments, order(-score)), ]
save(sentiments, file="data/sentiments.Rda")

# sna
EnsurePackage("igraph")
EnsurePackage("Matrix")
EnsurePackage("SparseM")
rt.df <- CreateSNADataFrame(df, from="from_user", to="retweet_from", linkNames="rt")
rp.df <- CreateSNADataFrame(df, from="from_user", to="reply_to", linkNames="rp")
sna.df <- rbind(rt.df, rp.df)
# create graph data frame (igraph)
g <- graph.data.frame(sna.df, directed=TRUE)
save(g, file="data/snaGraph.Rda")
mat <- get.adjacency(g)
# convert to csr matrix provided by SparseM ref:
# http://cos.name/cn/topic/108758
mat.csr <- as.matrix.csr(mat, ncol=ncol(mat))
save(mat.csr, file="data/snaMatrix.Rda")
g.wc <- walktrap.community(g, steps=1000, modularity=TRUE)
save(g.wc, file="data/communities.Rda")
