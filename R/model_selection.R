fselect <- function(y, data, alpha_in=0.01, alpha_out=0.05, mode="forward", early_break_count=10){
  if(mode=="forward"){
    # start
    {
      # Set alpha, in < out
      # define in and out set to save who should in model now
      df_in  <- data.frame()
      df_out <- data
      # Get the variable num and define no ones in df_in model
      out_width <- ncol(df_out)
      in_width  <- 0
      # loop run condition
      continue <- TRUE
      break_count <- 0
      who_in  <- ''
      who_out <- ''
    }
    
    while(continue){
      # when we into the loop, set continue = FALSE
      continue <- FALSE
      # create note and check it
      {
        # Create the empty vector to save names, F-values, P-values
        C_names <- c(); F_value <- c(); P_value <- c()
        # Do i times SLR create the information data.frame "note"
        for(i in 1:out_width){
          choose_data <- as.data.frame(df_out[,i])
          if(in_width!=0){
            test_f <- cbind(df_in, choose_data)
          }else{
            test_f <- choose_data
          }
          fit <- lm(y ~ ., test_f)
          aov <- anova(fit)
          C_names[i] <- colnames(df_out[i])
          F_value[i] <- aov$`F value`[in_width+1]
          P_value[i] <- aov$`Pr(>F)`[in_width+1]
        }
        note <- data.frame(name=C_names, F_value=F_value, P_value=P_value)
        # Find whos F-value is biggest and take it P-value
        max_F <- max(note$F_value)
        max_Fvar <- which(note$F_value==max_F)
        alpha_test <- as.numeric(note$P_value[max_Fvar])
      }
      
      # if it pass the check, add in and drop out
      {
        # Catch it in to model
        if(alpha_test <= alpha_in){
          # if the biggest F-value variable can be catch, we set countinue = TRUE
          continue <- TRUE
          # get the biggest F-value variable
          who_catch  <- note$name[max_Fvar]
          catch_var  <- which(colnames(df_out)==who_catch)
          catch_data <- as.data.frame(df_out[, catch_var])
          colnames(catch_data) <- who_catch
          # find it and add it from the df_out data.frame
          if(in_width!=0){
            df_in <- cbind(catch_data, df_in)
          }else{
            df_in <- as.data.frame(catch_data)
            colnames(df_in) <- who_catch
          }
          # find who into the df_in, and delete it from df_out
          if(out_width!=1){
            df_out[, who_catch] <- c()
          }else{
            df_out <- data.frame()
          }
          # since one variable move to df_in from df_out, change the width of df
          out_width <- out_width-1
          in_width  <- in_width +1
        }
      }
      cat('in:', in_width, '/ out:', out_width, ':', who_catch, '(IN) \n')
      who_in <- who_catch
      
      if(who_in==who_out){
        break_count <- break_count + 1
      }else{
        break_count <- 0
      }
      if(break_count > early_break_count){
        message_var <- paste("Break warning by variable in=out")
        warning(message_var)
        break
      }
      
      # stepwise part start
      # check df_in variable > 1
      if(in_width > 1){
        # delete this loop new variable
        if(in_width!=2){
          step_df <- df_in[,-1]
        }else{
          save_name <- colnames(df_in)[2]
          step_df <- as.data.frame(df_in[,-1])
          colnames(step_df) <- save_name
        }
        # create information of df_in model to "forward_note"
        forward_fit <- lm(y ~ ., step_df)
        # use another ones to anova
        forward_aov <- anova(forward_fit)
        C_names <- colnames(step_df)
        F_value <- forward_aov$`F value`[1:in_width-1] # in_width-1, since we delete this loop new variable
        P_value <- forward_aov$`Pr(>F)`[1:in_width-1] # in_width-1, since we delete this loop new variable
        forward_note <- data.frame(name=C_names, F_value=F_value, P_value=P_value)
        # Find whos F-value is smallest and take it P-value
        min_F <- min(forward_note$F_value)
        min_Fvar <- which(forward_note$F_value==min_F)
        alpha_test <- as.numeric(forward_note$P_value[min_Fvar])
        
        if(alpha_test > alpha_out){
          # if the smallest F-value variable can be drop, we set countinue = TRUE
          continue <- TRUE
          # get the smallest F-value variable
          who_catch <- forward_note$name[min_Fvar]
          drop_var  <- which(colnames(df_in)==who_catch)
          drop_data <- as.data.frame(df_in[, drop_var])
          colnames(drop_data) <- who_catch
          # find it and add it to the df_out data.frame
          if(out_width!=0){
            df_out <- cbind(drop_data, df_out)
          }else{
            df_out <- drop_data
          }
          # find who into the df_out, and delete it from df_in
          df_in[, drop_var] <- c()
          # since one variable move to df_out from df_in, change the width of df
          out_width <- out_width+1
          in_width  <- in_width -1
        }
      }
      cat('in:', in_width, '/ out:', out_width, ':', who_catch, '(OUT) \n')
      who_out <- who_catch
      
      # stepwise part end
     
      # if no any variable in df_out, stop loop
      if(out_width==0){
        break
      }
    }
  }
  if(mode=="backward"){
    # start
    {
      # Set alpha, in < out
      # define in and out set to save who should in model now
      df_in  <- data
      df_out <- data.frame()
      
      save <- alpha_in
      alpha_in  <- alpha_out
      alpha_out <- save
      
      # Get the variable num and define no ones in df_in model
      out_width <- 0
      in_width  <- ncol(df_in)
      
      # loop run condition
      continue <- TRUE
      break_count <- 0
      who_in  <- ''
      who_out <- ''
    }
    
    while(continue){
      # when we into the loop, set continue = FALSE
      continue <- FALSE
      
      # create note and check it
      {
        fit <- lm(y ~ ., df_in)
        aov <- anova(fit)
        # Create the  vector to save names, F-values, P-values
        C_names <- colnames(df_in)
        F_value <- aov[-(in_width+1),4]
        P_value <- aov[-(in_width+1),5]
        note <- data.frame(name=C_names, F_value=F_value, P_value=P_value)
        # Find whos F-value is smallst and take it P-value
        min_F <- min(note$F_value)
        min_Fvar <- which(note$F_value==min_F)
        alpha_test <- as.numeric(note$P_value[min_Fvar])
      }
      
      # if it pass the check, add in and drop out
      {
        # Drop it out to model
        if(alpha_test > alpha_in){
          # if the biggest F-value variable can be catch, we set countinue = TRUE
          continue <- TRUE
          # get the biggest F-value variable
          who_catch <- note$name[min_Fvar]
          drop_var  <- which(colnames(df_in)==who_catch)
          drop_data <- as.data.frame(df_in[, drop_var])
          colnames(drop_data) <- who_catch
          # find it and add it from the df_out data.frame
          if(out_width!=0){
            df_out <- cbind(drop_data, df_out)
          }else{
            df_out <- as.data.frame(drop_data)
            colnames(df_out) <- who_catch
          }
          # find who into the df_in, and delete it from df_out
          if(in_width!=1){
            df_in[, who_catch] <- c()
          }else{
            df_in <- data.frame()
          }
          # since one variable move to df_in from df_out, change the width of df
          out_width <- out_width+1
          in_width  <- in_width -1
        }
      }
      cat('in:', in_width, '/ out:', out_width, ':', who_catch, '(OUT) \n')
      who_out <- who_catch
      
      # stepwise part start
      # check df_out variable > 1
      if(out_width > 1){
        # delete this loop new variable
        if(out_width!=2){
          step_df <- df_out[,-1]
        }else{
          save_name <- colnames(df_out)[2]
          step_df <- as.data.frame(df_out[,-1])
          colnames(step_df) <- save_name
        }
        # create information of df_out model to "backward_note"
        forward_fit <- lm(y ~ ., step_df)
        # use another ones to anova
        forward_aov <- anova(forward_fit)
        C_names <- colnames(step_df)
        F_value <- forward_aov$`F value`[1:out_width-1] # out_width-1, since we delete this loop new variable
        P_value <- forward_aov$`Pr(>F)`[1:out_width-1] # out_width-1, since we delete this loop new variable
        backward_note <- data.frame(name=C_names, F_value=F_value, P_value=P_value)
        # Find whos F-value is biggest and take it P-value
        max_F <- max(backward_note$F_value)
        max_Fvar <- which(backward_note$F_value==max_F)
        alpha_test <- as.numeric(backward_note$P_value[max_Fvar])
        
        if(alpha_test <= alpha_out){
          # if the biggest F-value variable can be catch, we set countinue = TRUE
          continue <- TRUE
          # get the smallest F-value variable
          who_catch  <- backward_note$name[max_Fvar]
          catch_var  <- which(colnames(df_out)==who_catch)
          catch_data <- as.data.frame(df_out[, catch_var])
          colnames(catch_data) <- who_catch
          # find it and add it to the df_in data.frame
          if(in_width!=0){
            df_in <- cbind(catch_data, df_in)
          }else{
            df_in <- catch_data
          }
          # find who into the df_in, and delete it from df_out
          df_out[, catch_var] <- NULL
          # since one variable move to df_in from df_out, change the width of df
          out_width <- out_width-1
          in_width  <- in_width +1
        }
      }
      cat('in:', in_width, '/ out:', out_width, ':', who_catch, '(IN) \n')
      who_in <- who_catch
      
      if(who_in==who_out){
        break_count <- break_count + 1
      }else{
        break_count <- 0
      }
      if(break_count > early_break_count){
        message_var <- paste("Break warning by variable in=out")
        warning(message_var)
        break
      }
      
      # stepwise part end
      
      # if no any variable in df_out, stop loop
      if(out_width==0){
        break
      }
    }
  }
  # vif check part
  {
    fit <- lm(y ~ ., df_in)
    vif_value <- as.data.frame(car::vif(fit))
    who_catch <- vif_value[vif_value[,1]==max(vif_value),]
    name <- rownames(vif_value)[which(vif_value[,1]==who_catch)]
    
    if(vif_value[which(vif_value[,1]==who_catch),1]>=10){
      message_vif <- paste(name, "have 'Severe' Multicollinearity problem")
      warning(message_vif)
    }else if(vif_value[which(vif_value[,1]==who_catch),1]>=5){
      message_vif <- paste(name, "maybe have Multicollinearity problem")
      warning(message_vif)
    }else{
      print("Vif test pass!")
    }
    
  }
  return(fit)
}
