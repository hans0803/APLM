fselect <- function(y, data, alpha_in=0.01, alpha_out=0.05, mode="forward"){
  if(mode=="forward"){
    # start
    {
      # Set alpha, in < out
      # define in and out set to save who should in model now
      df_in <- data.frame(NULL)
      df_out <- data
      # Get the variable num and define no ones in df_in model
      out_width <- dim(df_out)[2]
      in_width <- 0
      # loop run condition
      continue <- TRUE
    }

    while(continue){
      # when we into the loop, set continue = FALSE
      continue <- FALSE

      # create note and check it
      {
        # Create the empty vector to save names, F-values, P-values
        C_names <- c(NULL)
        F_value <- c(NULL)
        P_value <- c(NULL)
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
        max_F <- max(note[,2])
        alpha_test <- as.numeric(note[note[,2]==max_F,3])
      }

      # if it pass the check, add in and drop out
      {
        # Catch it in to model
        if(alpha_test <= alpha_in){
          # if the biggest F-value variable can be catch, we set countinue = TRUE
          continue <- TRUE
          # get the biggest F-value variable
          who_catch <- note[note[,2]==max_F,1]
          catch_data <- as.data.frame(df_out[,which(colnames(df_out)==who_catch)])
          colnames(catch_data) <- who_catch
          # find it and add it from the df_out data.frame
          if(in_width!=0){
            df_in <- cbind(catch_data, df_in)
          }else{
            df_in <- catch_data
            save_name <- who_catch
          }
          # find who into the df_in, and delete it from df_out
          if(out_width!=1){
            df_out[, who_catch] <- NULL
          }else{
            df_out <- data.frame(NULL)
          }
          # since one variable move to df_in from df_out, change the width of df
          out_width <- out_width-1
          in_width <- in_width+1
        }
      }

      # stepwise part start
      # check df_in variable > 1
      if(in_width > 1){
        # delete this loop new variable
        if(in_width!=2){
          step_df <- df_in[,-1]
        }else{
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
        min_F <- min(forward_note[,2])
        alpha_test <- as.numeric(forward_note[forward_note[,2]==min_F,3])
        if(alpha_test > alpha_out){
          # if the smallest F-value variable can be drop, we set countinue = TRUE
          continue <- TRUE
          # get the smallest F-value variable
          who_catch <- forward_note[note[,2]==min_F,1]
          catch_data <- as.data.frame(df_in[,which(colnames(df_in)==who_catch)])
          # find it and add it to the df_out data.frame
          if(dim(df_out)[2]!=0){
            df_out <- cbind(catch_data, df_out)
          }else{
            df_out <- catch_data
          }
          # find who into the df_out, and delete it from df_in
          df_in[, which(colnames(df_in)==who_catch)] <- NULL
          # since one variable move to df_out from df_in, change the width of df
          out_width <- out_width+1
          in_width <- in_width-1
        }
      }
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
      df_in <- data
      df_out <- data.frame(NULL)

      save <- alpha_in
      alpha_in <- alpha_out
      alpha_out <- save

      # Get the variable num and define no ones in df_in model
      out_width <- 0
      in_width <- dim(df_in)[2]
      # loop run condition
      continue <- TRUE
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
        min_F <- min(note[,2])
        alpha_test <- as.numeric(note[note[,2]==min_F,3])
      }

      # if it pass the check, add in and drop out
      {
        # Drop it out to model
        if(alpha_test >= alpha_in){
          # if the biggest F-value variable can be catch, we set countinue = TRUE
          continue <- TRUE
          # get the biggest F-value variable
          who_catch <- note[note[,2]==min_F,1]
          catch_data <- as.data.frame(df_in[,which(colnames(df_in)==who_catch)])
          colnames(catch_data) <- who_catch
          # find it and add it from the df_out data.frame
          if(out_width!=0){
            df_out <- cbind(catch_data, df_out)
          }else{
            df_out <- catch_data
            save_name <- who_catch
          }
          # find who into the df_in, and delete it from df_out
          if(in_width!=1){
            df_in[, who_catch] <- NULL
          }else{
            df_in <- data.frame(NULL)
          }
          # since one variable move to df_in from df_out, change the width of df
          out_width <- out_width+1
          in_width <- in_width-1
        }
      }

      # stepwise part start
      # check df_in variable > 1
      if(out_width > 1){
        # delete this loop new variable
        if(out_width!=2){
          step_df <- df_out[,-1]
        }else{
          step_df <- as.data.frame(df_out[,-1])
          colnames(step_df) <- save_name
        }
        # create information of df_in model to "forward_note"
        forward_fit <- lm(y ~ ., step_df)
        # use another ones to anova
        forward_aov <- anova(forward_fit)
        C_names <- colnames(step_df)
        F_value <- forward_aov$`F value`[1:out_width-1] # in_width-1, since we delete this loop new variable
        P_value <- forward_aov$`Pr(>F)`[1:out_width-1] # in_width-1, since we delete this loop new variable
        forward_note <- data.frame(name=C_names, F_value=F_value, P_value=P_value)
        # Find whos F-value is smallest and take it P-value
        max_F <- max(forward_note[,2])
        alpha_test <- as.numeric(forward_note[forward_note[,2]==max_F,3])
        if(alpha_test > alpha_out){
          # if the smallest F-value variable can be drop, we set countinue = TRUE
          continue <- TRUE
          # get the smallest F-value variable
          who_catch <- forward_note[note[,2]==max_F,1]
          catch_data <- as.data.frame(df_out[,which(colnames(df_out)==who_catch)])
          # find it and add it to the df_out data.frame
          if(dim(df_in)[2]!=0){
            df_in <- cbind(catch_data, df_in)
          }else{
            df_in <- catch_data
          }
          # find who into the df_out, and delete it from df_in
          df_out[, which(colnames(df_out)==who_catch)] <- NULL
          # since one variable move to df_out from df_in, change the width of df
          out_width <- out_width-1
          in_width <- in_width+1
        }
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
    }

  }
  return(fit)
}

aselect <- function(y, data){

  # save x names
  x_name <- colnames(data)
  # create empty list sub to save every combn x
  sub <- list(NULL)
  # create empty list sub_name to save every combn x_name
  sub_name <- list(NULL)
  # save total combination of every x type
  num <- c(NULL)

  # do col of x times loop to combn x, x_name and save num
  for(i in 1:dim(data)[2]){

    sub[[i]] <- combn(data, i)
    sub_name[[i]] <- combn(x_name, i)
    num[i] <- dim(sub[[i]])[2]

  }

  # unlist sub_name to vector
  sub_name <- unlist(sub_name)
  # define k to count the list size
  k <- 1
  # create empty list sub to save combination of x
  df_list <- list(NULL)

  for(i in 1:dim(data)[2]){ # x col loop
    for(j in 1:num[i]){ # x type loop

      df_list[[k]] <- as.data.frame(sub[[i]][,j]) # list->data.frame, it will use in lm
      para_length <- length(colnames(df_list[[k]])) # get ? para in this data.frame
      colnames(df_list[[k]]) <- sub_name[1:para_length] # gave para name from sub_name
      sub_name <- sub_name[-1:-para_length] # delete used name
      k <- k+1

    }
  }

  # create empty vector to save aic value
  aic <- c(NULL)
  # do data.frame size loop by Baska triangle
  total <- 2**dim(data)[2]-1

  for(i in 1:total){

    df <- as.data.frame(cbind(y, df_list[[i]])) # cbind y and x
    aic[i] <- AIC(lm(df[,1] ~ ., as.data.frame(df[,-1]))) # do aic and save it

  }

  # return the minimal aic value parameter's model names
  fit <- lm(y ~ ., df_list[[which(aic==min(aic))]])
  return(fit)
}
