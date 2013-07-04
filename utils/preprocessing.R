## Preprocessing
#  To speed up the application, this script is trying to do 
#  those weight-lefting tasks in advance, and save processed
#  data in advance.

source("utils/utilities.R", local=TRUE)
source("utils/get_tweets.R")
source("utils/munge_tweets.R")
source("utils/semantic_analysis.R")
source("utils/social_analysis.R")

# get tweets
settings <- read.table("settings.txt", header=FALSE)
key <- settings[2, 1]
timezone <- settings[3, 1]
df <- GetTweetsFromGoogleDrive(key, tz=timezone)

# remove duplicates
df <- df[!duplicated(df$id_str), ]
# # only select tweets during conference
# start <- strptime("10/04/2013 00:00:01", "%d/%m/%Y %H:%M:%S")
# df <- df[(df$created_at > start), ]
df <- PreprocessTweets(df)

# create corpus
corpus <- ConstructCorpus(df$text_nourl, removeTags=TRUE, removeUsers=TRUE)

# sentiments
sentiments <- ScoreSentiment(df$text_nourl, .progress="text")
sentiments$from_user <- df$from_user
sentiments <- sentiments[with(sentiments, order(-score)), ]

# sna
EnsurePackage("igraph")
EnsurePackage("Matrix")
EnsurePackage("SparseM")
rt.df <- CreateSNADataFrame(df, from="from_user", to="retweet_from", linkNames="rt")
rp.df <- CreateSNADataFrame(df, from="from_user", to="reply_to", linkNames="rp")
sna.df <- rbind(rt.df, rp.df)
# create graph data frame (igraph)
g <- graph.data.frame(sna.df, directed=TRUE)
mat <- get.adjacency(g)
# convert to csr matrix provided by SparseM ref:
# http://cos.name/cn/topic/108758
mat.csr <- as.matrix.csr(mat, ncol=ncol(mat))
g.wc <- walktrap.community(g, steps=1000, modularity=TRUE)

## save data
save(df, file="data/df.Rda")
write.csv(df, file="data/df.csv")
save(corpus, file="data/corpus.Rda")
save(sentiments, file="data/sentiments.Rda")
save(g, file="data/snaGraph.Rda")
save(mat.csr, file="data/snaMatrix.Rda")
save(g.wc, file="data/communities.Rda")
