##########################################################################################
# 
##########################################################################################
methods::setClass(
  Class = "EmpiricalIRT",
  slots = list(
    name = "character",
    item = "character",
    option = "numeric",
    position = "numeric", #1:left 2:midle 3:right
    x = "numeric" , #vector
    y1 = "numeric", #vector
    y1_max = "numeric",
    y2 = "numeric", #vector
    y3 = "numeric", #vector
    information = "numeric", #vector
    monotonicity = "numeric",
    logit1 = "ANY", # grm regression model
    logit2 = "ANY", # grm regression model
    logit3 = "ANY", # grm regression model
    responses1 = "numeric", #vector
    responses2 = "numeric", #vector
    responses3 = "numeric" #vector
  )
)
##########################################################################################
# 
##########################################################################################
methods::setClass(
  Class = "EmpiricalIRTModel",
  slots = list(
    data = "data.frame",
    sum_scores = "numeric",
    sum_scores_mean = "numeric",
    sum_scores_sd = "numeric",
    scores_axis = "vector",
    z_scores_axis = "vector",
    items = "character",
    irts = "list"
  )
)
##########################################################################################
# 
##########################################################################################
setMethod("plot",
          "EmpiricalIRTModel",
          function(x, type="ICC",item,option=NULL, xlim=NULL, ylim=NULL, draw.points=TRUE, draw.lines=TRUE, draw.logit=TRUE, draw.logit_method=0, main=NULL, add_score_axis = FALSE, ...) {
            model<- x;
            option_plot_flag = FALSE
            if (is.null(item) || is.na(item)){
              stop("item expected")
            }
            p_irts = model@irts[[item]]
            irt_seclected<-NULL
            if (is.null(p_irts)){ stop(paste("canot find item",item)) }
            if (! is.null(option)){
              option_irt = NULL;
              for (irt in p_irts){
                if (option == irt@option){
                  option_plot_flag = TRUE;
                  #p_irts=c(irt)
                  irt_seclected = irt;
                  break()
                }
              }
              if (! option_plot_flag){
                stop(paste('canot find option:',option,'for item:', item))
              }
            }
            if (length(p_irts) == 0){
              return
            }
            
            xlab='z-score'
            if (is.null(xlim)) {
              x_min <- min(model@z_scores_axis)
              x_max <- max(model@z_scores_axis)
              xlim <- c(x_min, x_max)
            }
            
            
            if (option_plot_flag){
              plot_title<-paste(item,'option:',option)
            } else{
              plot_title<-paste(item)
            }
            if (!is.null(main)){
              plot_title<-paste(main,':',plot_title)
            }
            
            
            # if (type == 'INF'){
            #   if (is.null(irt_seclected)){
            #     stop('INF without option not supported');
            #   }
            #   plot_title<-paste(plot_title, 'information')
            #   irt <- irt_seclected
            #
            #   if (is.null(ylim)){
            #     tmp<-irt@information[complete.cases(irt@information)]
            #     imin<-min(tmp)
            #     imax<-max(tmp)
            #     ylim<-c(imin,imax)
            #   }
            #   graphics::plot(NA,  xlab = xlab,  ylab = 'information',  main = plot_title,  xlim = xlim,  ylim = ylim, labels=xlabels, ...  )
            #   color='black'
            #   if (draw.points){
            #     points(irt@x, irt@information,col=color, pch=20)
            #   }
            #   if (draw.lines){
            #     lines(irt@x, irt@information,col=color)
            #   }
            #   grid()
            #   return()
            # }
            
            
            if (is.null(ylim)){
              ylim<-c(0,1.1)
            }
            
            if (draw.logit && draw.logit_method > 0){
              plot_title<-paste(plot_title, 'logit:', draw.logit_method)
            }
            
            graphics::plot(NA,  xlab = xlab,  ylab = 'probability',  main = plot_title,  xlim = xlim,  ylim = ylim, ...   )
            if (add_score_axis) {
              axis(1, at=model@z_scores_axis, labels=model@scores_axis, pos=1.1)
            }
            
            
            
            if(option_plot_flag){
              colors2<-rep('red',times=length(p_irts))
              colors<-rep('black',times=length(p_irts))
            } else {
              if (draw.lines){
                colors2<-rep('gray',times=length(p_irts))
                colors<-rainbow(n=length(p_irts))
              } else {
                colors2<-rainbow(n=length(p_irts))
                colors<-rep('black',times=length(p_irts))
              }
            }
            
            
            ci<-1
            len = length(p_irts)
            for (i in 1:len){
              irt<-p_irts[[i]]
              if (! option_plot_flag || irt@option == option){
                color<-colors[ci]
                if (draw.points){
                  points(irt@x, irt@y1,col=color, pch=20)
                }
                if (draw.lines){
                  lines(irt@x, irt@y1,col=color)
                }
                
                
                #Logistic regression
                if (!is.null(irt@logit1) && !is.null(irt@logit2) && !is.null(irt@logit3) && draw.logit){
                  logit_x <- seq(xlim[1], xlim[2], .05)
                  
                  if (draw.logit_method ==1){
                    logit_y1 <- predict(irt@logit1, list(x = logit_x), type = "response");
                    logit_y = logit_y1
                  } else if (draw.logit_method ==2){
                    logit_y2 <- predict(irt@logit2, list(x = logit_x), type = "response");
                    logit_y = logit_y2
                  } else if (draw.logit_method ==3){
                    logit_y3 <- predict(irt@logit3, list(x = logit_x), type = "response");
                    logit_y = logit_y3
                  }else {
                    logit_y2 <- predict(irt@logit2, list(x = logit_x), type = "response");
                    logit_y3 <- predict(irt@logit3, list(x = logit_x), type = "response");
                    logit_y0 <- (1 -logit_y3 -logit_y2)
                    logit_y = logit_y0
                  }
                  lines(logit_x, logit_y, col = colors2[ci])
                  
                  #information
                  #color='pink'
                  #if (draw.points){
                  #  points(irt@x, irt@information,col=color, pch=20)
                  #}
                  #if (draw.lines){
                  #  lines(irt@x, irt@information,col=color)
                  #}
                }#/Logistic regression
              }
              ci <-ci + 1
            }
            
            abline(v=0,col='gray2')
            txt <- if (! option_plot_flag) '' else paste('monotonicity:', round(irt_seclected@monotonicity, 4))
            txt <- paste(txt,'nrow:', nrow(model@data))
            mtext(txt, side = 3, line = 0, adj = 1, font = 1, cex=0.9)
            grid()
          })
##########################################################################################
# 
##########################################################################################
#' @title 
#' @param data 
#' @param items 
#' @param addlogit
#' @importFrom 
#' @keywords assumptions
#' @export
#' @examples
psych::sim.rasch(nvar = 10,n = 1000, low=-3,high=3,d=NULL, a=1,mu=0,sd=1)
psych::sim.irt(nvar = 10, n = 1000, low=-3, high=3,a=NULL,c=0,z=1,d=NULL,mu=0,sd=1,mod="logistic")
psych::sim.poly(nvar = 10 ,n = 1000, low=-2,high=2,a=NULL,c=0,z=1,d=NULL, mu=0,sd=1,cat=5,mod="logistic") 
irt_empirical_model <- function(data = NULL, items = NULL,   addlogit = TRUE) {
  if (is.null(items)) {
    items <- names(data)
  } else {
    data <- data[items]
  }
  
  sum_scores <- rowSums(data)
  sum_scores_mean = mean(sum_scores)
  sum_scores_sd = sd(sum_scores)
  score_max=max(sum_scores)
  score_min=min(sum_scores)
  scores_range = seq(score_min, score_max,by=1)
  scores_z <- (scores_range - sum_scores_mean) / sum_scores_sd
  
  #print(scores_z)
  
  ####
  items_info <- list()
  for (item in items) {
    tmp1 = data[[item]]
    min <- min(tmp1)
    max <- max(tmp1)
    len <- length(unique(tmp1))
    irts_vector <- list()
    for (i in seq(1, max, by = 1)) {
      item_name <- paste(item, ' option: ', i,  sep = '')
      v1 <- tmp1
      v1[v1 != i] <- 0
      
      v2 <- tmp1
      v2[v2 <= i] <- 0
      
      v3 <- tmp1
      v3[v3 >= i] <- 0
      
      
      if (i == 1){
        position = 1
      } else if ( i == max){
        position = 3
      } else {
        position =2
      }
      irt <- new(
        "EmpiricalIRT",
        name = item_name,
        x = scores_z,
        item = item,
        option = i,
        position = position,
        responses1 = v1,
        responses2 = v2,
        responses3 = v3
      )
      irts_vector <- c(irts_vector,irt)
      # }
    }
    
    items_info[[item]]<-irts_vector
  }
  #data <- do.call(data.frame, response_list)
  #colnames(data) <- col_names
  ####
  
  #print(head(data))
  #print(head(items_info))
  items <- names(data)
  
  for (j in items) {
    irts<-items_info[[j]]
    if (is.null(irts)){
      cat(paste("SKIP ",j))
      next
    }
    irts_ok=c()
    #print(typeof(irts))
    #print(head(irts))
    for (irt in irts){
      probs1 = c()
      probs2 = c()
      probs3 = c()
      information =c()
      for (i in score_min:score_max) {
        index1 <-(sum_scores == i)
        my_responses1 <- irt@responses1[index1] #simplirosis me sumscore == i
        my_responses2 <- irt@responses2[index1] #simplirosis me sumscore == i
        my_responses3 <- irt@responses3[index1] #simplirosis me sumscore == i
        prob1 <- (length(my_responses1[my_responses1 > 0]) / length(my_responses1)) # gia to sigkekrimeno score i ti simpliro8ike sto item
        prob2 <- (length(my_responses2[my_responses2 > 0]) / length(my_responses2)) # gia to sigkekrimeno score i ti simpliro8ike sto item
        prob3 <- (length(my_responses3[my_responses3 > 0]) / length(my_responses3)) # gia to sigkekrimeno score i ti simpliro8ike sto item
        #if (is.na(prob1)){ prob1 = 0; }
        #if (is.na(prob2)){ prob2 = 0; }
        probs1<-c(probs1,prob1)
        probs2<-c(probs2,prob2)
        probs3<-c(probs3,prob3)
        #print(prob1 * (1-prob1))
        information = c(information, (prob1 * (1-prob1)))
        
      }
      #print(information)
      #println('lens',length(irt@x),length(probs))
      irt@y1 <- probs1
      irt@y1_max <- max(probs1,na.rm=TRUE)
      irt@y2 <- probs2
      irt@y3 <- probs3
      irt@information <- information
      #println('len1',irt@name,length(irt@y1),length(irt@y2))
      if (addlogit) {
        m_logit = glm(
          formula = y ~ x,
          data = data.frame(y = probs1, x = scores_z),
          family = quasibinomial
        )
        irt@logit1 = m_logit
        
        m_logit = glm(
          formula = y ~ x,
          data = data.frame(y = probs2, x = scores_z),
          family = quasibinomial
        )
        irt@logit2 = m_logit
        
        m_logit = glm(
          formula = y ~ x,
          data = data.frame(y = probs3, x = scores_z),
          family = quasibinomial
        )
        irt@logit3 = m_logit
        
        
      }
      
      
      mcor2 <- if (all(probs2 == 0 | is.na(probs2)))  NA else cor(scores_z, probs2,  method = "spearman", use="pairwise")
      mcor3 <- if (all(probs3 == 0 | is.na(probs3)))  NA else cor(scores_z, probs3,  method = "spearman", use="pairwise")
      if (! is.na(mcor2) && ! is.na(mcor3)){
        mcor <- (abs(mcor2) + abs(mcor3))/2
      } else {
        mcor <- cor(scores_z, probs1,  method = "spearman", use="pairwise")
      }
      
      if (FALSE && irt@item == 'Pers_obedient'){
        print('------------------------------------------------------')
        print(irt@name)
        # print("1:")
        # print(probs1)
        # print("2:")
        # print(probs2)
        # print("3:")
        # print(probs3)
        
        #mcor1 <- cor(scores_z, probs1,  method = "spearman", use="pairwise.complete.obs")
        #mcor2 <- cor(scores_z, probs2,  method = "spearman", use="pairwise.complete.obs")
        #mcor3 <- cor(scores_z, probs3,  method = "spearman", use="pairwise.complete.obs")
        mcor1 <- cor(scores_z, probs1,  method = "spearman", use="pairwise")
        mcor2 <- cor(scores_z, probs2,  method = "spearman", use="pairwise")
        mcor3 <- cor(scores_z, probs3,  method = "spearman", use="pairwise")
        mcor4 <- (abs(mcor2) + abs(mcor3))/2
        
        print(paste('cor1',mcor1))
        print(paste('cor2',mcor2))
        print(paste('cor3',mcor3))
        print(paste('cor4',mcor4))
        print('------------------------------------------------------')
      }
      
      irt@monotonicity <-mcor
      irts_ok<-c(irts_ok,irt)
    }
    items_info[[j]]<-irts_ok
  }
  
  my_model <- new("EmpiricalIRTModel",
                  data = data,
                  sum_scores = sum_scores,
                  sum_scores_mean = sum_scores_mean,
                  sum_scores_sd = sum_scores_sd,
                  z_scores_axis = scores_z,
                  scores_axis = scores_range,
                  items=items,
                  irts=items_info
  )
  
  # for (item in my_model@items){
  #   #println('t1',item)
  #   irts <- my_model@irts[[item]]
  #   for (irt in irts){
  #     println('len2',irt@name, length(irt@y1),length(irt@y2))
  #   }
  # }
  return(my_model)
  
}



