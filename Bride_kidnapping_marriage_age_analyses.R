library( dagitty )
library( readxl )
library( dplyr )
library( tidyr )
library( rethinking )
library( ggplot2 )
library( ggdist )
library( gt )
library( gto )
library( officer )
library( cmdstanr )

#2 Marriage age ----
##2.1 DAG ----
Dag_MA <- 
  dagitty( 'dag {
bb="-0.5,-0.5,0.5,0.5"
"Marriage age" [outcome,pos="0.000,0.200"]
Age [pos="-0.150,-0.330"]
Brideprice [pos="-0.200,0.100"]
Cur.wealth [pos="0.200,0.100"]
Gender [pos="0.004,-0.406"]
His.wealth [latent,pos="-0.270,-0.160"]
Marriage [exposure,pos="0.150,-0.330"]
Religiosity [pos="0.270,-0.160"]
U_village [latent,pos="0.120,0.200"]
Age -> "Marriage age"
Age -> Brideprice
Age -> His.wealth
Age -> Marriage
Age -> Religiosity
Brideprice -> Cur.wealth
Gender -> "Marriage age"
His.wealth -> "Marriage age"
His.wealth -> Brideprice
His.wealth -> Cur.wealth
Marriage -> "Marriage age"
Marriage -> Brideprice
Religiosity -> "Marriage age"
Religiosity -> Marriage
U_village -> "Marriage age"
}')

adjustmentSets( Dag_MA , 
                exposure = "Marriage" , 
                outcome = "Marriage age" ,
                effect = "total" )

##2.2 Data loading ----
load( file = "Bride_kidnapping_marriage_age_analyses.RData" )
  
##2.3 Models ----
Model_list_MA <- with( MA.data , list(
  MA = MA28 ,
  Status = Event , 
  VID = as.integer( VID ) , 
  Gender = as.integer( Gender ) ,
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) ,
  Pray = as.integer( Pray ) ) )

{set.seed(123)
  Model_MG_MA <- ulam(
    alist(
      MA|Status == 1 ~ exponential( lambda ) ,
      MA|Status == 0 ~ custom( exponential_lccdf( !Y|lambda ) ) ,
      lambda <- 1.0/mu ,
      log(mu) <- bMG[Kidnap,Gender] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts 
      
      matrix[Kidnap,Gender]: bMG ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]:V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0 , 0.5 ) ,
      sigma_V ~ exponential( 2 ) 
    ) , data = Model_list_MA , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MG_MA <- precis( Model_MG_MA , depth = 3 , prob = 0.90 , 
                           pars = c( "bMG" , "bA" , "bR" , "V" ) )
Pre_model_MG_MA

##2.4 Figures ----
Post_model_MG_MA <- extract.samples( Model_MG_MA )
Post_model_MGV_MA <- extract.samples( Model_MGV_MA )

###2.4.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 1 , 2 ) , oma = c( 2.2 , 3.2 , 0.2 , 0.2 ) , mar = c( 1.4 , 0.4 , 0 , 0 ) )
  dens( Post_model_MG_MA$bMG[,1,1] , 
        adj = 1 , 
        xlim = c( -1.8 , 1.8 ) ,
        ylim = c( 0 , 2.0 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#e47178" , cex.axis = 1.2 )
  dens( Post_model_MG_MA$bMG[,2,1] ,
        adj = 1 , 
        lwd = 3 , col = "#e47178" , 
        add = T )
  mtext( "Density" , side = 2 , line = 2.4 , cex = 1.5 )
  mtext( "Females" , side = 1 , line = 2.4 , cex = 1.5 )
  
  dens( Post_model_MG_MA$bMG[,1,2] ,
        adj = 1 , 
        xlim = c( -1.8 , 1.8 ) ,
        ylim = c( 0 , 2.0 ) ,
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#a0add0" , cex.axis = 1.2 )
  dens( Post_model_MG_MA$bMG[,2,2] ,
        adj = 1 , 
        lwd = 3 , col = "#a0add0" , 
        add = T )
  legend( x = 0.05 , y = 2.1 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" , 
                      "Non-kidnapping male" , 
                      "Kidnapping male" ) , 
          lty = c( 3 , 1 , 3 , 1 ) ,  
          col = c( "#e47178" , "#e47178" , "#a0add0" , "#a0add0" ) , 
          lwd = 2 ,
          cex = 1 , 
          bty = "n" ,
          y.intersp = 1.0 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
  mtext( "Males" , side = 1 , line = 2.4 , cex = 1.5 )
}

dev.off()

###2.4.2 Predicted marriage age ----
# Function to predict p-matrix based on posterior samples of estimates and new data
predict_p_matrix_MA_MG <- function( post_samples , gender , kidnap , 
                                    time_point = 0:18 , 
                                    age_range = 1:4 , 
                                    pray_range = 1:2 , 
                                    vid_range = 1:2 ,
                                    n_samples = 6000 ) {
  new_data <- expand.grid( Kidnap = kidnap ,
                           Gender = gender ,
                           Age = age_range ,
                           Pray = pray_range ,
                           VID = vid_range )
  
  N <- nrow( new_data )
  N_Time <- length( time_point )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = N_Time )
  
  for ( s in 1:n_samples ) {
    bMG <- post_samples$bMG[s,,]    # [Kidnap, Gender]
    bA <- post_samples$bA[s,]       # [Age]
    bR <- post_samples$bR[s,]       # [Pray]
    V_s <- post_samples$V_bar[s] + post_samples$z_V[s, new_data$VID] * post_samples$sigma_V[s]
    
    for (t in 1:N_Time) {
      mu <- exp( bMG[cbind(new_data$Kidnap, new_data$Gender)] +
                   bA[new_data$Age] +
                   bR[new_data$Pray] +
                   V_s )
      lambda <- 1 / mu
      surv_probs <- exp(-lambda * time_point[t])
      p_matrix[s, t] <- mean(surv_probs)  # averaged over combinations
    }
  }
  
  colnames(p_matrix) <- paste0("t", time_point+10)
  return(p_matrix)
}

set.seed(123)
{
  p_matrix_MA_MG_female_nonkid <- predict_p_matrix_MA_MG( Post_model_MG_MA , gender = 1 , kidnap = 1 ) 
  p_matrix_MA_MG_female_nonkid <- p_matrix_MA_MG_female_nonkid %>%
    as_tibble() %>%
    mutate( across( everything() ) ) %>% 
    {
      t10_values <- pull( ., t10 )
      new_cols <- matrix( rep( t10_values , 9 ) , ncol = 9 ,
                          dimnames = list( NULL , paste0( "t" , 1:9 ) ) )
      cbind( new_cols , . )  
    } %>%
    magrittr::set_colnames( c( paste0( "t" , 1:9 ) , colnames( p_matrix_MA_MG_female_nonkid ) ) )
  
  p_matrix_MA_MG_female_kid <- predict_p_matrix_MA_MG( Post_model_MG_MA , gender = 1 , kidnap = 2 )
  p_matrix_MA_MG_female_kid <- p_matrix_MA_MG_female_kid %>%
    as_tibble() %>%
    mutate( across( everything() ) ) %>% 
    {
      t10_values <- pull( ., t10 )
      new_cols <- matrix( rep( t10_values , 9 ) , ncol = 9 ,
                          dimnames = list( NULL , paste0( "t" , 1:9 ) ) )
      cbind( new_cols , . )  
    } %>%
    magrittr::set_colnames( c( paste0( "t" , 1:9 ) , colnames( p_matrix_MA_MG_female_kid ) ) )
  
  p_matrix_MA_MG_male_nonkid <- predict_p_matrix_MA_MG( Post_model_MG_MA , gender = 2 , kidnap = 1 )
  p_matrix_MA_MG_male_nonkid <- p_matrix_MA_MG_male_nonkid %>%
    as_tibble() %>%
    mutate( across( everything() ) ) %>% 
    {
      t10_values <- pull( ., t10 )
      new_cols <- matrix( rep( t10_values , 9 ) , ncol = 9 ,
                          dimnames = list( NULL , paste0( "t" , 1:9 ) ) )
      cbind( new_cols , . )  
    } %>%
    magrittr::set_colnames( c( paste0( "t" , 1:9 ) , colnames( p_matrix_MA_MG_male_nonkid ) ) )
  
  p_matrix_MA_MG_male_kid <- predict_p_matrix_MA_MG( Post_model_MG_MA , gender = 2 , kidnap = 2 )
  p_matrix_MA_MG_male_kid <- p_matrix_MA_MG_male_kid %>%
    as_tibble() %>%
    mutate( across( everything() ) ) %>% 
    {
      t10_values <- pull( ., t10 )
      new_cols <- matrix( rep( t10_values , 9 ) , ncol = 9 ,
                          dimnames = list( NULL , paste0( "t" , 1:9 ) ) )
      cbind( new_cols , . )  
    } %>%
    magrittr::set_colnames( c( paste0( "t" , 1:9 ) , colnames( p_matrix_MA_MG_male_kid ) ) )
}


{ par( mfrow = c( 1 , 2 ) , oma = c( 3.6 , 3.6 , 0.2 , 0.2 ) , mar = c( 0 , 0.4 , 0 , 0 ) )
  # Female
  plot( NULL ,
        xlim = c( 1, 28 ) ,
        ylim = c( 0 , 1 ) ,
        xlab = "" ,
        ylab = "" ,
        cex.axis = 1.2 )
  # non-kidnapping
  lines( c( 1:28 ) , 
         apply( p_matrix_MA_MG_female_nonkid , 2 , mean ) ,
         lwd = 3 , col = "#f5dc75" )
  shade( apply( p_matrix_MA_MG_female_nonkid , 2 , PI , prob = 0.30 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#f5dc75" , 0.25 ) )
  shade( apply( p_matrix_MA_MG_female_nonkid , 2 , PI , prob = 0.60 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#f5dc75" , 0.25 ) )
  shade( apply( p_matrix_MA_MG_female_nonkid , 2 , PI , prob = 0.90 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#f5dc75" , 0.25 ) )
  
  # kidnapping
  lines( c( 1:28 ) , 
         apply( p_matrix_MA_MG_female_kid , 2 , mean ) ,
         lwd = 3 , col = "#e47178" )
  shade( apply( p_matrix_MA_MG_female_kid , 2 , PI , prob = 0.30 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#e47178" , 0.25 ) )
  shade( apply( p_matrix_MA_MG_female_kid , 2 , PI , prob = 0.60 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#e47178" , 0.25 ) )
  shade( apply( p_matrix_MA_MG_female_kid , 2 , PI , prob = 0.90 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#e47178" , 0.25 ) )
  mtext( "Predicted probability of remaing unmarried" , 
         side = 2 , line = 2.5 , cex = 1.5 )
  mtext( "Age in years" , side = 1 , line = 2.5 , cex = 1.5 )
  
  # male
  plot( NULL ,
        xlim = c( 1, 28 ) ,
        ylim = c( 0 , 1 ) ,
        yaxt = "n" , 
        xlab = "" ,
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 )
  # non-kidnapping
  lines( c( 1:28 ) , 
         apply( p_matrix_MA_MG_male_nonkid , 2 , mean ) ,
         lwd = 3 , col = "#75c298" )
  shade( apply( p_matrix_MA_MG_male_nonkid , 2 , PI , prob = 0.30 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#75c298" , 0.25 ) )
  shade( apply( p_matrix_MA_MG_male_nonkid , 2 , PI , prob = 0.60 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#75c298" , 0.25 ) )
  shade( apply( p_matrix_MA_MG_male_nonkid , 2 , PI , prob = 0.90 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#75c298" , 0.25 ) )
  # kidnapping
  lines( c( 1:28 ) , 
         apply( p_matrix_MA_MG_male_kid , 2 , mean ) ,
         lwd = 3 , col = "#a0add0" )
  shade( apply( p_matrix_MA_MG_male_kid , 2 , PI , prob = 0.30 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#a0add0" , 0.25 ) )
  shade( apply( p_matrix_MA_MG_male_kid , 2 , PI , prob = 0.60 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#a0add0" , 0.25 ) )
  shade( apply( p_matrix_MA_MG_male_kid , 2 , PI , prob = 0.90 ) , 
         c( 1:28 ) , 
         col = col.alpha( "#a0add0" , 0.25 ) )
  legend( x = 15 , y = 1.05 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" , 
                      "Non-kidnapping male" , 
                      "Kidnapping male" ) , 
          lty = c( 1 , 1 , 1 , 1 ) ,  
          col = c( "#f5dc75" , "#e47178" , "#75c298" , "#a0add0" ) , 
          lwd = 2 ,
          cex = 1.2 , 
          bty = "n" ,
          y.intersp = 1.0 ,
          x.intersp = 0.4 ,
          seg.len = 1.0  )
  mtext( "Age in years" , side = 1 , line = 2.5 , cex = 1.5 )
}

dev.off()

##2.5 Outputs ----
###2.5.1 Estimates ----
Pre_MG_MA_output <- data.frame( Mean = Pre_model_MG_MA$mean ,
                                CI5 = Pre_model_MG_MA$`5%` ,
                                CI95 = Pre_model_MG_MA$`95%` ,
                                Variable = c( "Non-kidnapped female" , 
                                              "Kidnapped female" , 
                                              "Non-kidnapping male" , 
                                              "Kidnapping male" , 
                                              "Age: <40" , "Age: 40-49" , 
                                              "Age: 50-59" , "Age: >=60" , 
                                              "Pray: seldom" , "Pray: often" , 
                                              "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Non-kidnapped female" , 
                                         "Kidnapped female" , 
                                         "Non-kidnapping male" , 
                                         "Kidnapping male" , 
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Non-kidnapped female" , 
                                         "Kidnapped female" , 
                                         "Non-kidnapping male" , 
                                         "Kidnapping male" , 
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Marriage age" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

MG_MA_output <- Pre_MG_MA_output %>% 
  select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 150 ) )

MG_MA_output

###2.5.2 Estimate differences ----
tibble( Gender = c( "Woman" , "Man" ) ,
        bM_diff = c( sprintf("%0.2f" , mean( Post_model_MG_MA$bMG[,2,1] - Post_model_MG_MA$bMG[,1,1] ) ) , 
                     sprintf("%0.2f" , mean( Post_model_MG_MA$bMG[,2,2] - Post_model_MG_MA$bMG[,1,2] ) ) ) ,
        
        bM_diff_L = c( sprintf("%0.2f" , PI( Post_model_MG_MA$bMG[,2,1] - Post_model_MG_MA$bMG[,1,1] , prob = 0.90 )[1] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_MA$bMG[,2,2] - Post_model_MG_MA$bMG[,1,2] , prob = 0.90 )[1] ) ) ,
        
        bM_diff_H = c( sprintf("%0.2f" , PI( Post_model_MG_MA$bMG[,2,1] - Post_model_MG_MA$bMG[,1,1] , prob = 0.90 )[2] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_MA$bMG[,2,2] - Post_model_MG_MA$bMG[,1,2] , prob = 0.90 )[2] ) ) ,
        `bM_diff[90%CI]` = paste( bM_diff , "[" , bM_diff_L , ", " , bM_diff_H , "]") , 
        Pro_bM_diff = c( sprintf( "%0.4f" , 
                                  length( which( ( Post_model_MG_MA$bMG[,2,1] - Post_model_MG_MA$bMG[,1,1] ) > 0 ) ) / 6000 ) , 
                         sprintf( "%0.4f" , 
                                  length( which( ( Post_model_MG_MA$bMG[,2,2] - Post_model_MG_MA$bMG[,1,2] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bM_diff = paste( round( as.numeric( Pro_bM_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( Gender , `bM_diff[90%CI]` , Pro_bM_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Gender , `bM_diff[90%CI]` , Pro_bM_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )
