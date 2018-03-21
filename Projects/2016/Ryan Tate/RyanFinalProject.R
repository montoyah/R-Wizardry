rm(list=ls(all=T))

#Ryan Tate
#Final Project

tree.node<-function(play=NA,node="start.node",act=NA,pay=NA,prob=NA,tree,remove.node=FALSE){ # inputs and defaults
  if(remove.node){ # checks to see if removing node
    for(node.i in 1:length(node)){ # cycles through nodes that are being removed
      if(length(which(tree[[2]]==node[node.i]))>0){ # checkes to see if those nodes exist
        tree[[1]][which(tree[[2]]==node[node.i])]<-NA # replaces all of the node's info with NA's
        tree[[3]][[which(tree[[2]]==node[node.i])]]<-NA
        tree[[4]][[which(tree[[2]]==node[node.i])]]<-NA
        tree[[5]][[which(tree[[2]]==node[node.i])]]<-NA
        tree[[2]][which(tree[[2]]==node[node.i])]<-NA
      }else{
        print(paste("Node",node[node.i],"does not exist",sep=" ")) # warning if node doesn't exist
      }
    }
  }else{ # if adding nodes
    outcome<-NA
    for(node.i in 1:length(node)){ # cycles through nodes that are being added
      if(!is.na(act[1])){ # checks if node has actions
        if(length(node)>1){ # checks to see if more than one node is being added
          for(act.i in 1:length(act)){
            outcome[act.i]<-paste(act[act.i],node[node.i],sep="~") # creates unique action names for multi-node info sets
          }
          print(outcome) # prints unique actions so they can be used for the next node
        }else{
          outcome<-act # if only one node it transfers act to outcome
        }
      }
      if(length(which(tree[[2]]==node[node.i]))>0){ # if node already exist it will overwrite that node
        tree[[1]][which(tree[[2]]==node[node.i])]<-play
        tree[[3]][[which(tree[[2]]==node[node.i])]]<-outcome
        tree[[4]][[which(tree[[2]]==node[node.i])]]<-pay
        tree[[5]][[which(tree[[2]]==node[node.i])]]<-prob
      }else{
        if(length(which(is.na(tree[[2]])))>0){ # if a node is NA this will overwrite it
          tree[[1]][which(is.na(tree[[2]]))[1]]<-play
          tree[[3]][[which(is.na(tree[[2]]))[1]]]<-outcome
          tree[[4]][[which(is.na(tree[[2]]))[1]]]<-pay
          tree[[5]][[which(is.na(tree[[2]]))[1]]]<-prob
          tree[[2]][which(is.na(tree[[2]]))[1]]<-node[node.i]
        }else{
          tree[[1]][length(tree[[1]])+1]<-play # if there is no node to overwite this will create node to add to the list
          tree[[2]][length(tree[[1]])]<-node[node.i]
          tree[[3]][[length(tree[[1]])]]<-outcome
          tree[[4]][[length(tree[[1]])]]<-pay
          tree[[5]][[length(tree[[1]])]]<-prob
        }
      }
    }
  }
  return(tree) # returns the decision tree
}

tree.draw<-function(tree,equilibrium=NA){ # draws a tree given a decision tree, equilibrium is used for solving function to pass solutions to this function
  for(node.i in 1:length(tree[[2]])){ # this checks to see your tree is complete
    if(!is.na(tree[[2]][node.i])){
      check.i<-0
      for(node.j in 1:length(tree[[2]])){
        if(!is.na(tree[[3]][[node.j]][1])){
          for(act in tree[[3]][[node.j]]){
            if(length(which(tree[[2]]==act))==0){ # checks that all actions lead to a corresponding node
              print(paste("ERROR! Action ",act," does not connect to a node"))
              return()
              
            }else{
              if(tree[[2]][node.i]==act) # checks that the node had a corespoding action
                check.i<-1
            }
          }
        }
      }
      if(check.i==0){
        if(tree[[2]][node.i]!="start.node"){ # checks that the node had a corespoding action, but makes an exception for the start.node
          print(paste("ERROR! Node ",tree[[2]][node.i]," does not connect to an action"))
          return()
        }
      }
    }
  }
  order<-list("start.node")
  connections<-list(NA)
  info.set<-list(NA)
  round.i<-0
  while(round.i<length(order)){ # builds list for determining structure of the tree
    round.i<-round.i+1
    sequence.i<-1
    for(node.i in 1:length(order[[round.i]])){
      if(is.na(tree[[3]][[which(tree[[2]]==order[[round.i]][node.i])]][1])){ # this is for dead end nodes
        connections[[round.i]][node.i]<-NA
        info.set[[round.i+1]]<-NA
      }else{
        connections[[round.i]][node.i]<-length(tree[[3]][[which(tree[[2]]==order[[round.i]][node.i])]]) # this is for connecting node
        if(any(grepl("~",tree[[3]][[which(tree[[2]]==order[[round.i]][node.i])]]))){ # this is for marking information sets
          info.set[[round.i]][node.i]<-1
        }else{
        }
        if(length(order)==round.i){ # this is for making the next slice of the tree
          order[[round.i+1]]<-NA
          connections[[round.i+1]]<-NA
          info.set[[round.i+1]]<-NA
        }
        for(act in tree[[3]][[which(tree[[2]]==order[[round.i]][node.i])]]){ # this is for determing what nodes are in the next slice of the tree
          order[[round.i+1]][sequence.i]<-act
          sequence.i<-sequence.i+1
        }
      }
    }
  }
  plot(0,xlim=c(1,length(order)+0.5),ylim=c(1,0),col=0,axes=FALSE) # creates empty plot, note that the y axis is flipped
  info.set.i<-0
  for(round.i in 1:(length(order))){# cycles through slices of the tree
    segment.i<-1
    for(node.i in 1:length(order[[round.i]])){# cycles through nodes in a slice
      if(!is.na(connections[[round.i]][node.i])){ # node text and point
        text(x=round.i,y=node.i/(length(order[[round.i]])+1)-0.05,labels=tree[[1]][which(tree[[2]]==order[[round.i]][node.i])]) # -0.05 is offset
        points(x=round.i,y=node.i/(length(order[[round.i]])+1),pch=19)
        if(!is.na(info.set[[round.i]][node.i])){ # checks if part of info set
          if(info.set.i>0){ # checks if another info set has been found
            segments(x0=round.i,y0=node.i/(length(order[[round.i]])+1),x1=info.set.node[1],y1=info.set.node[2],lty=3) # connects nodes in info set
          }else{
            info.set.i<-1
            info.set.node<-c(round.i,node.i/(length(order[[round.i]])+1)) # stores info about node in info set
          }
        }
        for(act.i in 1:connections[[round.i]][node.i]){
          text.i<-strsplit(tree[[3]][[which(tree[[2]]==order[[round.i]][node.i])]][act.i],"~")
          if(length(which(equilibrium==text.i[[1]][1]))>0){
            arrows(x0=round.i,y0=node.i/(length(order[[round.i]])+1),x1=round.i+1,y1=segment.i/(length(order[[round.i+1]])+1),lwd=2) # arrow for equilibrium choice
          }else{
            segments(x0=round.i,y0=node.i/(length(order[[round.i]])+1),x1=round.i+1,y1=segment.i/(length(order[[round.i+1]])+1)) # default line
          }
          if(!is.na(tree[[5]][[which(tree[[2]]==order[[round.i]][node.i])]][act.i])){ # text for probabilities
            text.i<-strsplit(text.i[[1]][1],"`")
            text.i<-paste(text.i[[1]][1]," p=",tree[[5]][[which(tree[[2]]==order[[round.i]][node.i])]][act.i])
            text(x=(round.i+round.i+1)/2,y=(node.i/(length(order[[round.i]])+1)+segment.i/(length(order[[round.i+1]])+1))/2-0.025,labels=text.i) # -0.025 is offset
          }else{
            text.i<-strsplit(text.i[[1]][1],"`") # text for actions
            text(x=(round.i+round.i+1)/2,y=(node.i/(length(order[[round.i]])+1)+segment.i/(length(order[[round.i+1]])+1))/2-0.025,labels=text.i[[1]][1]) # -0.025 is offset
          }
          segment.i<-segment.i+1
        }
      }else{ # text for probabilities
        text(x=round.i+0.25,y=node.i/(length(order[[round.i]])+1),labels=paste(tree[[4]][[which(tree[[2]]==order[[round.i]][node.i])]],collapse=", ")) # +0.25 is offset
      }
    }
  }
}

tree.solveNE<-function(tree){ # just requires decsion tree
  for(node.i in 1:length(tree[[2]])){ # this checks to see your tree is complete
    if(!is.na(tree[[2]][node.i])){
      check.i<-0
      for(node.j in 1:length(tree[[2]])){
        if(!is.na(tree[[3]][[node.j]][1])){
          for(act in tree[[3]][[node.j]]){
            if(length(which(tree[[2]]==act))==0){ # checks that all actions lead to a corresponding node
              print(paste("ERROR! Action ",act," does not connect to a node"))
              return()
            }else{
              if(tree[[2]][node.i]==act) # checks that the node had a corespoding action
                check.i<-1
            }
          }
        }
      }
      if(check.i==0){
        if(tree[[2]][node.i]!="start.node"){ # checks that the node had a corespoding action, but makes an exception for the start.node
          print(paste("ERROR! Node ",tree[[2]][node.i]," does not connect to an action"))
          return()
        }
      }
    }
  }
  node.act<-list(NA)
  node.act.i<-1
  imp.info<-NA
  for(node.i in 1:length(tree[[2]])){ # cycles through node
    if(is.na(tree[[5]][[node.i]][1])){ # is it a non-probability node?
      if(!is.na(tree[[3]][[node.i]][1])){ # does is have an action?
        if(any(grepl("~",tree[[3]][[node.i]]))){ # is it part of an info set
          act<-strsplit(tree[[3]][[node.i]][1],"~")
          if(!is.na(match(act[[1]][1],imp.info))){ # has the info set already been recorded?
            break
          }else{ # if not mark that info set has been recorded
            for(act.i in 1:length(tree[[3]][[node.i]])){
              act<-strsplit(tree[[3]][[node.i]][act.i],"~")
              imp.info<-c(imp.info,act[[1]][1])
            }
          }
        }
        node<-NA
        for(act.i in 1:length(tree[[3]][[node.i]])){ # record each action in node
          act<-strsplit(tree[[3]][[node.i]][act.i],"~")
          node[act.i]<-act[[1]][1]
        }
        node.act[[node.act.i]]<-node # store those actions, grouped by node
        node.act.i<-node.act.i+1
      }
    }
  }
  temp.table<-expand.grid(node.act,stringsAsFactors=FALSE)
  decision.table<-as.data.frame(matrix(data=NA,nrow=length(temp.table[,1]),ncol=length(temp.table[1,])+max(tree[[1]],na.rm=TRUE)+1))
  decision.table[1:length(temp.table[,1]),1:length(temp.table[1,])]<-temp.table # create table that has all possible decision combinations
  for(row.i in 1:length(decision.table[,1])){ # for each combination find out payoff
    temp.tree<-tree # so tree can be rest for each combination
    while(is.na(temp.tree[[4]][[which(temp.tree[[2]]=="start.node")]][1])){ # cycle while no payoff for combination
      for(node.i in 1:length(temp.tree[[2]])){ # cylces through nodes
        if(is.na(temp.tree[[4]][[node.i]][1])){ # does it have no payoff?
          if(!is.na(temp.tree[[3]][[node.i]][1])){ # does is have actions?
            if(!is.na(temp.tree[[5]][[node.i]][1])){ # is it a probability node
              for(act in temp.tree[[3]][[node.i]]){ # cycles through actions in node
                if(is.na(temp.tree[[4]][[which(temp.tree[[2]]==act)]][1])){ # checks to see if all actions have payoffs
                  break
                }
                if(act==temp.tree[[3]][[node.i]][length(temp.tree[[3]][[node.i]])]){ # if all actions have payoffs
                  temp.tree[[4]][[node.i]]<-0
                  for(prob.i in 1:length(temp.tree[[5]][[node.i]])){ # deterimines payoff based on probability of each action
                    temp.tree[[4]][[node.i]]<-temp.tree[[4]][[node.i]]+temp.tree[[4]][[which(temp.tree[[2]]==temp.tree[[3]][[node.i]][prob.i])]]*temp.tree[[5]][[node.i]][prob.i]
                  }
                }
              }
            }else{ # if not probability node
              for(act in temp.tree[[3]][[node.i]]){ # cycles through actions in node
                act.s<-strsplit(act,"~")
                if(!is.na(match(act.s[[1]][1],temp.table[row.i,]))){ # if action matches a decision then copy payoff from that action to this node
                  temp.tree[[4]][[node.i]]<-temp.tree[[4]][[which(temp.tree[[2]]==act)]]
                }
              }
            }
          }
        }
      }
    }
    decision.table[row.i,(length(temp.table)+1):(length(decision.table[1,])-1)]<-temp.tree[[4]][[which(temp.tree[[2]]=="start.node")]] # transfer decision payoff to table
  }
  col.names<-NA
  for(col.i in 1:length(temp.table)){ # cycles through columns that contain decisions
    for(node.i in 1:length(tree[[2]])){ # cycles through nodes
      if(!is.na(tree[[3]][[node.i]][1])){ # does the node have actions?
        act<-strsplit(tree[[3]][[node.i]],"~")
        if(!is.na(match(decision.table[1,col.i],act[[1]][1]))){ # is one of the actions in this column?
          col.names[col.i]<-tree[[1]][[node.i]] # determines player who controls that node's decisions
          break
        }
      }
    }
  }
  colnames(temp.table)<-col.names # rename decision column for temp.table with player who controls that node's decisions
  for(row.i in 1:(length(decision.table[,1]))){ # checking to see if each row is a Nash Equilibrium
    for(row.j in 1:length(decision.table[,1])){ # cycle through row that it will be compared against
      if(max(tree[[1]],na.rm=TRUE)==1){ # checks to see if there is only one player
        decision.table[which(decision.table[,length(temp.table)+1]!=max(decision.table[,length(temp.table)+1])),length(decision.table[1,])]<-1 # if so mark row i if lower than row j
      }else{
        for(play.i in 1:max(tree[[1]],na.rm=TRUE)){ # cycles through players
          if(all(decision.table[row.i,which(colnames(temp.table)!=play.i)]==decision.table[row.j,which(colnames(temp.table)!=play.i)])){ # are all different action under this player's control
            if(max(decision.table[row.i,length(temp.table)+play.i],decision.table[row.j,length(temp.table)+play.i])!=decision.table[row.i,length(temp.table)+play.i]){ # is row i lower than row j for this player?
              decision.table[row.i,length(decision.table[1,])]<-1 # if so mark row i
            }
          }
        }
      }
    }
  }
  if(length(which(is.na(decision.table[,length(decision.table[1,])])))>0){ # are there any unmarked rows?
    for(NE.i in which(is.na(decision.table[,length(decision.table[1,])]))){ # cycle through all unmarked rows and pass equlibria to the draw function
      NE<-as.vector(decision.table[NE.i,1:(length(temp.table[1,]))])
      tree.draw(tree=tree,equilibrium=NE)
    }
    NE.table<-as.data.frame(matrix(data=NA,nrow=length(which(is.na(decision.table[,length(decision.table[1,])]))),ncol=length(decision.table[1,])-1))
    NE.table.i<-1 # creating a nicer Nash equilbrium table to return
    for(NE.i in which(is.na(decision.table[,length(decision.table[1,])]))){ # cycle through NE
      for(col.i in 1:length(temp.table)){
        text.i<-strsplit(decision.table[NE.i,col.i],"`") # this removes unique action markers that are unwanted in the output
        decision.table[NE.i,col.i]<-text.i[[1]][1]
      }
      NE.table[NE.table.i,]<-decision.table[NE.i,1:length(NE.table[1,])] # transfers NE from old table to new table
      NE.table.i<-NE.table.i+1
    }
    colnames(NE.table)<-c(col.names,paste("p",1:max(tree[[1]],na.rm=TRUE),sep="")) # rename decision column for NE.table with player who controls that node's decisions and recieve each payoff
    return(NE.table) # return all NE and their payoffs
  }else{ # if all rows are marked
    print("No pure Nash Equilibrium")
    return()
  }
}

tree.solveSPE<-function(tree){ # just requires decsion tree
  for(node.i in 1:length(tree[[2]])){ # this checks to see your tree is complete
    if(!is.na(tree[[2]][node.i])){
      check.i<-0
      for(node.j in 1:length(tree[[2]])){
        if(!is.na(tree[[3]][[node.j]][1])){
          for(act in tree[[3]][[node.j]]){
            if(length(which(tree[[2]]==act))==0){ # checks that all actions lead to a corresponding node
              print(paste("ERROR! Action ",act," does not connect to a node"))
              return()
              
            }else{
              if(tree[[2]][node.i]==act) # checks that the node had a corespoding action
                check.i<-1
            }
          }
        }
      }
      if(check.i==0){
        if(tree[[2]][node.i]!="start.node"){ # checks that the node had a corespoding action, but makes an exception for the start.node
          print(paste("ERROR! Node ",tree[[2]][node.i]," does not connect to an action"))
          return()
        }
      }
    }
  }
  if(any(grepl("~",tree[[2]]))){ # checks to see if there are any multi-node info sets
    print("There is imperfect information, Can't solve for SPE")
    return()
  }
  SPE<-list(NA,NA)
  SPE.i<-1
  while(is.na(tree[[4]][[which(tree[[2]]=="start.node")]][1])){ # cycles until there is a payoff for the start.node
    for(node.i in 1:length(tree[[2]])){ # cycles through nodes
      pay<-list(NA)
      pay.i<-1
      if(is.na(tree[[4]][[node.i]][1])){ # does this node have a payoff?
        if(!is.na(tree[[3]][[node.i]][1])){ # does this node have actions?
          for(act in tree[[3]][[node.i]]){ # cycles through actions
            if(is.na(tree[[4]][[which(tree[[2]]==act)]][1])){ # checks to see if all the node's actions have payoffs
              break
            }
            if(tree[[1]][node.i]>0){ # makes sure nature isn't controling this node
              pay[[1]][pay.i]<-tree[[4]][[which(tree[[2]]==act)]][tree[[1]][node.i]] # adds this node's player's payoff for each action
              pay.i<-pay.i+1
            }
            if(act==tree[[3]][[node.i]][length(tree[[3]][[node.i]])]){ # if all the node's actions have payoffs
              if(!is.na(tree[[5]][[node.i]][1])){ # check to see if probability node
                tree[[4]][[node.i]]<-0
                for(prob.i in 1:length(tree[[5]][[node.i]])){ # deterimines payoff based on probability of each action
                  tree[[4]][[node.i]]<-tree[[4]][[node.i]]+tree[[4]][[which(tree[[2]]==tree[[3]][[node.i]][prob.i])]]*tree[[5]][[node.i]][prob.i]
                }
              }else{
                if(length(which(pay[[1]]==max(pay[[1]])))>1){ # check to see if there is a tie
                  print("There is a tie, can't solve for a single SPE") # fuction can't handle ties
                  return()
                }else{ # otherwise compare node's player's payoff for each action to select which action player would do
                  tree[[4]][[node.i]]<-tree[[4]][[which(tree[[2]]==tree[[3]][[node.i]][which(pay[[1]]==max(pay[[1]]))])]] # copy that action's payoff to this node
                  SPE[[1]][SPE.i]<-tree[[3]][[node.i]][which(pay[[1]]==max(pay[[1]]))] # record action decided
                  SPE[[2]][SPE.i]<-tree[[1]][node.i] # record player that chose this action
                  SPE.i<-SPE.i+1
                }
              }
            }
          }
        }
      }
    }
  }
  tree.draw(tree=tree,equilibrium=SPE[[1]]) # pass decided action to draw function
  for(SPE.i in 1:length(SPE[[1]])){
    text.i<-strsplit(SPE[[1]][SPE.i],"`") # this removes unique action markers that are unwanted in the output
    SPE[[1]][SPE.i]<-text.i[[1]][1]
  }
  SPE.table<-as.data.frame(matrix(data=NA,nrow=1,ncol=length(SPE[[1]])+max(tree[[1]],na.rm=TRUE))) # create table for output
  SPE.table[1,length(SPE[[1]]):1]<-SPE[[1]] # adds decisions to table
  SPE.table[1,(length(SPE[[1]])+1):length(SPE.table[1,])]<-tree[[4]][[which(tree[[2]]=="start.node")]] # adds payoff
  colnames(SPE.table)<-c(SPE[[2]][length(SPE[[2]]):1],paste("p",1:max(tree[[1]],na.rm=TRUE),sep="")) # adds players who control each decision and recieve each payoff
  return(SPE.table) # return SPE and it's payoff
}

game<-list(NA,NA,list(NA),list(NA),list(NA))
game<-tree.node(play=1,act=c("cooperate`1","defect`1"),tree=game)
game<-tree.node(play=2,node=c("cooperate`1","defect`1"),act=c("cooperate`2","defect`2"),tree=game)
game<-tree.node(node="cooperate`2~cooperate`1",pay=c(3,3),tree=game)
game<-tree.node(node="cooperate`2~defect`1",pay=c(5,0),tree=game)
game<-tree.node(node="defect`2~cooperate`1",pay=c(0,5),tree=game)
game<-tree.node(node="defect`2~defect`1",pay=c(1,1),tree=game)

tree.draw(game)
tree.solveNE(game)
tree.solveSPE(game)

game2<-list(NA,NA,list(NA),list(NA),list(NA))
game2<-tree.node(play=1,act=c("heads`1","tails`1"),tree=game2)
game2<-tree.node(play=2,node=c("heads`1","tails`1"),act=c("heads`2","tails`2"),tree=game2)
game2<-tree.node(node="heads`2~heads`1",pay=c(1,0),tree=game2)
game2<-tree.node(node="tails`2~heads`1",pay=c(0,1),tree=game2)
game2<-tree.node(node="heads`2~tails`1",pay=c(0,1),tree=game2)
game2<-tree.node(node="tails`2~tails`1",pay=c(1,0),tree=game2)

tree.draw(game2)
tree.solveNE(game2)
tree.solveSPE(game2)

game3<-list(NA,NA,list(NA),list(NA),list(NA))
game3<-tree.node(play=1,act=c("pass`1","steal`1"),tree=game3)
game3<-tree.node(play=2,node="pass`1",act=c("pass`2","steal`2"),tree=game3)
game3<-tree.node(play=1,node="pass`2",act=c("pass`3","steal`3"),tree=game3)
game3<-tree.node(play=2,node="pass`3",act=c("share","steal`4"),tree=game3)
game3<-tree.node(node="steal`1",pay=c(1,0),tree=game3)
game3<-tree.node(node="steal`2",pay=c(0,10),tree=game3)
game3<-tree.node(node="steal`3",pay=c(100,0),tree=game3)
game3<-tree.node(node="share",pay=c(500,500),tree=game3)
game3<-tree.node(node="steal`4",pay=c(0,1000),tree=game3)

tree.draw(game3)
tree.solveNE(game3)
tree.solveSPE(game3)

game4<-list(NA,NA,list(NA),list(NA),list(NA))
game4<-tree.node(play=1,act=c("raise","stay"),tree=game4)
game4<-tree.node(play=2,node=c("raise"),act=c("fold","call"),tree=game4)
game4<-tree.node(play=0,node=c("stay"),act=c("win`stay","lose`stay"),prob=c(0.5,0.5),tree=game4)
game4<-tree.node(play=0,node=c("call"),act=c("win`call","lose`call"),prob=c(0.5,0.5),tree=game4)
game4<-tree.node(node="win`stay",pay=c(1,-1),tree=game4)
game4<-tree.node(node="lose`stay",pay=c(-1,1),tree=game4)
game4<-tree.node(node="fold",pay=c(1,-1),tree=game4)
game4<-tree.node(node="win`call",pay=c(2,-2),tree=game4)
game4<-tree.node(node="lose`call",pay=c(-2,2),tree=game4)

tree.draw(game4)
tree.solveNE(game4)
tree.solveSPE(game4)

game5<-list(NA,NA,list(NA),list(NA),list(NA))
game5<-tree.node(play=1,act=c("captain","firstmate","deckhand"),tree=game5)
game5<-tree.node(play=2,node="captain",act=c("kill`c2","accept`c2"),tree=game5)
game5<-tree.node(play=2,node="firstmate",act=c("kill`f2","accept`f2"),tree=game5)
game5<-tree.node(play=2,node="deckhand",act=c("kill`d2","accept`d2"),tree=game5)
game5<-tree.node(play=3,node="kill`c2",act=c("kill`c3","accept`c3"),tree=game5)
game5<-tree.node(play=3,node="kill`f2",act=c("kill`f3","accept`f3"),tree=game5)
game5<-tree.node(play=3,node="kill`d2",act=c("kill`d3","accept`d3"),tree=game5)
game5<-tree.node(node="accept`c2",pay=c(2,0,0),tree=game5)
game5<-tree.node(node="accept`f2",pay=c(0,2,0),tree=game5)
game5<-tree.node(node="accept`d2",pay=c(0,0,2),tree=game5)
game5<-tree.node(node="kill`c3",pay=c(-10,3,1),tree=game5)
game5<-tree.node(node="accept`c3",pay=c(2,-1,0),tree=game5)
game5<-tree.node(node="kill`f3",pay=c(-10,3,1),tree=game5)
game5<-tree.node(node="accept`f3",pay=c(0,1,0),tree=game5)
game5<-tree.node(node="kill`d3",pay=c(-10,3,1),tree=game5)
game5<-tree.node(node="accept`d3",pay=c(0,-1,2),tree=game5)

tree.draw(game5)
tree.solveNE(game5)
tree.solveSPE(game5)
