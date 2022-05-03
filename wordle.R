#Load words list
words = read.csv("C:/Users/scotta/Downloads/twl06.txt",header = F)
View(words)
str(words)

#Calculate length of each word
lenwords = as.data.frame(lapply(words,nchar))

#Create list of words of length 5
words5 = words[lenwords == 5]

#function to compare two words and return result
comp = function(guess,answr){
  guess = match(strsplit(guess,'')[[1]],letters) #Turn word into vector of numbers
  answr = match(strsplit(answr,'')[[1]],letters)
  p = sum(guess == answr) #Number of perfect matches
  if(p == 0){
    return(0)
  }
  result = c(0,0,0,0,0) #Vector of score values
  for(i in 1:5){ #Iterate through letters in guess
    if(!(guess[i] %in% answr)){ #If the letter is not in the answer, go to the next letter
      next
    } else if(guess[i] == answr[i]){ #Check for perfect match
      result[i] = 2
      next
    } else {
      result[i] = 1
      if(length(unique(guess)) == 5){ #Check for repeated letters in guess
        next
      } else if(sum(result[guess[i] == guess] == 1) > sum(guess[i] == answr) - sum(answr[guess == answr] == guess[i])){
        #If the repeated letter is not needed, then mark as 0
        result[i] = 0
      }
    }
  }
  return(sum(result*c(10000,1000,100,10,1))) #Return result as a ternary value
}

comp2 = function(guess,answr){ #Alternate scoring method that is about the same speed
  guess = match(strsplit(guess,'')[[1]],letters)
  answr = match(strsplit(answr,'')[[1]],letters)
  if(sum(guess == answr) == 0){
    return(0)
  }
  result = c(0,0,0,0,0) #Vector of score values
  incorrect = c(T,T,T,T,T) #Vector of which letters are not perfect matches
  unused = c(T,T,T,T,T) #Vector of which letters are not in answer
  for(i in 1:5){
    if(guess[i] == answr[i]){ #Check for perfect matches
      result[i] = 2
      incorrect[i] = F
      unused[i] = F
    }
  }
  for(i in 1:5){
    if(incorrect[i]){ #Only check letters that are not perfect matches
      for(j in 1:5){  #Look for match in answer for each unused letter
        if(unused[j] & answr[i] == guess[j]){ 
          result[j] = 1
          unused[j] = F
        }
      }
    }
  }
  return(sum(result*c(10000,1000,100,10,1)))
}

bank = words5[1:2000] #Set number of words to use (affects time to process)
wordScores = matrix(nrow = length(bank), ncol = length(bank), dimnames = list(bank,bank)) #Create matrix to store scores
start.time = Sys.time()
for(i in 1:length(bank)){
  for(j in 1:length(bank)){
    wordScores[i,j] = comp(bank[i],bank[j]) #Score every guess word for every answer word
  }
}
stop.time = Sys.time()
stop.time - start.time
#Takes around 2 hours to run!

#Load saved result of word scores
wordScores <- read.csv("C:/Users/scotta/My Drive/R/wordle/wordCompare.csv", 
                        header=TRUE, 
                        row.names=1)

wordScores <- as.matrix(wordScores)
View(wordScores)

# ternTodec = function(x){
#   s = rev(strsplit(as.character(x),"")[[1]])
#   d = 0
#   for(i in 1:length(s)){
#     d = d + as.integer(s[i])*3^(i-1)
#   }
#   return(d)
# }
# 
# 
# for(a in 1:length(wordScores)){
#   wordScores[a] = ternTodec(wordScores[a])
# }

bank = words5

#Function to find which guess word eliminates the most possible words on average
analyze.possible = function(possible){
  power = matrix(nrow = length(bank), ncol = length(possible), dimnames = list(bank,bank[possible]))
  n = length(possible)
  for(ans in 1:n){
    for(gus in 1:length(bank)){
      power[gus,ans] = n - sum(wordScores[gus,possible] == wordScores[gus,possible[ans]])
    }
  }
  return(which.max(rowMeans(power)))
}

possible = 1:length(bank)

#Find which initial guess eliminates the most words on average
start.time = Sys.time()
power = matrix(nrow = length(bank), ncol = length(possible), dimnames = list(bank,bank[possible]))
n = length(possible)
for(ans in 1:n){
  for(gus in 1:length(bank)){
    power[gus,ans] = n - sum(wordScores[gus,possible] == wordScores[gus,possible[ans]])
  }
}
stop.time = Sys.time()
stop.time - start.time
#Takes almost 3 hours!


#Load saved result of power
power = read.csv("C:/Users/scotta/My Drive/R/wordle/power.csv",
                 header = T,
                 row.names = 1)
power = as.matrix(power)

#Find which words have the largest elimination average
wordPower = rowMeans(power)
head(sort(wordPower, decreasing = T), n = 50)
#Best word by this metric is "lares"

#Find max partition size for each word
maxPartition = unlist(lapply(apply(wordScores,1,table),max))
head(sort(maxPartition), n = 50)
#Best word (smallest max) by this metric is "serai"

#Find average partition size for each word
avgPartition = unlist(lapply(apply(wordScores,1,table),mean))
head(sort(avgPartition), n = 50)
#Best word (smallest mean) by this metric is "tares"

#Find number of partitions for each word (same as average partition size)
nPartition = unlist(lapply(apply(wordScores,1,table),length))
head(sort(nPartition,decreasing = T), n = 50)
#Best word (largest number of partitions) by this metric is "tares"

#Find average value of "bits" for each word
expInfo = function(x){
  s = sum(x)
  return(sum(-x/s*log(x/s, base = 2)))
}
wordBits = unlist(lapply(apply(wordScores,1,table),expInfo))
head(sort(wordBits,decreasing = T), n = 50)
#Best word (most bits) by this metric is "tares"

#Use analysis to solve wordle
possible = 1:length(bank)
guess = "point"
score = 202 #Score for guess
possible = possible[wordScores[guess,possible] == score] #Identify possible words based on score
length(possible)
head(bank[possible], n = 10)
analyze.possible(possible)
bank[answer]

#Loop to automatically solve wordles
answer = sample(bank,1)
possible = 1:length(bank)
guess = "lares"
g = 1
correct = F
while(!correct & g<10){
  print(paste("Guess",g,":",guess))
  print(wordScores[guess,answer])
  if(guess == answer){
    correct = T
  }
  possible = possible[wordScores[guess,possible] == wordScores[guess,answer]]
  print(paste("Possible:",length(possible)))
  if(length(possible) == 1){
    guess = bank[possible]
  } else {
    guess = bank[analyze.possible(possible)]
  }
  g = g+1
}
print(answer)

#Find solution for each word
guessLength = data.frame(row.names = bank)
for(i in 1:length(bank)){
  answer = bank[i]
  possible = 1:length(bank)
  guess = "lares"
  g = 1
  guessLength[i,"g"] = g
  guessLength[i,"guess1"] = guess
  while(g<10){
    if(guess == answer){
      break
    }
    possible = possible[wordScores[guess,possible] == wordScores[guess,answer]]
    if(length(possible) == 1){
      guess = bank[possible]
    } else {
      guess = bank[analyze.possible(possible)]
    }
    g = g+1
    guessLength[i,paste("guess",g,sep = "")] = guess
  }
  guessLength[i,1] = g
  write.table(guessLength[i,], col.names = F)
}

#Visualize wordScores
w = 7679
barplot(sort(table(wordScores[w,]),decreasing = T),
        las=2,
        main = rownames(wordScores)[w],
        cex.names = 0.5)
par(mfrow = c(2,2))
while(w<10){
  barplot(sort(table(wordScores[w,]),decreasing = T),las=2,main = rownames(wordScores)[w])
  w = w+1
}
which(rownames(wordScores) == "tares")

#Add hover text to plot
#install.packages("plotly") if needed
library(plotly)
w = which(rownames(wordScores) == "slate")
partition = sort(table(wordScores[w,])/length(wordScores[1,]), decreasing = T)
plot_ly(x = names(partition),
        y = partition,
        type = "bar") %>%
  layout(title = rownames(wordScores)[w],
         xaxis = list(categoryorder = "total descending"))
