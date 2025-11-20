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

#1 Be nominated for various social relationships ----
##1.1 DAG ----
Dag_BN <- 
  dagitty( 'dag {
bb="-0.5,-0.5,0.5,0.5"
Age.cohort [pos="-0.290,-0.140"]
Be.nominated [outcome,pos="0.000,0.320"]
Cur.wealth [pos="0.290,-0.140"]
Gender [pos="-0.150,-0.315"]
Marriage [exposure,pos="0.150,-0.315"]
No.nomination [pos="-0.230,0.190"]
Religiosity [pos="0.230,0.190"]
U_village [latent,pos="0.180,0.320"]
Age.cohort -> Be.nominated
Age.cohort -> Marriage
Age.cohort -> No.nomination
Age.cohort -> Religiosity
Cur.wealth -> Be.nominated
Cur.wealth -> No.nomination
Gender -> Be.nominated
Gender -> No.nomination
Marriage -> Be.nominated
Marriage -> Cur.wealth
Marriage -> No.nomination
No.nomination -> Be.nominated
Religiosity -> Be.nominated
Religiosity -> Marriage
Religiosity -> No.nomination
U_village -> Be.nominated
}
')

adjustmentSets( Dag_BN , 
                exposure = "Marriage" , 
                outcome = "Be.nominated" ,
                effect = "total" )

##1.2 Data loading ----
load( file = "Bride_kidnapping_social_relationships_analyses.RData" )

##1.3 Models ----
###1.3.1 The number of being nominated for prestige ----
Model_M_list_pre <- with( BN.data , list(
  Nominated = as.integer( No.be.nominated.prestige ) ,
  VID = as.integer( VID ) , 
  Gender = as.integer( Gender ) ,
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) , 
  Pray = as.integer( Pray ) ) )

c( mean = mean( BN.data$No.be.nominated.prestige ) ,
   var = var( BN.data$No.be.nominated.prestige ) , 
   ratio = var( BN.data$No.be.nominated.prestige )/mean( BN.data$No.be.nominated.prestige ) ) # Sanity checks: over dispersion

{set.seed(123)
  Model_MG_pre <- ulam(
    alist(
      Nominated ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bMG[Kidnap,Gender] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts 
      
      matrix[Kidnap,Gender]: bMG ~ normal( 0 , 0.5 ) ,
      
      phi ~ exponential( 1 ) , 
      
      bA[Age] ~ normal( 0 , 0.5 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 0.8 , 0.5 ) ,
      z_V[VID] ~ normal( 0 , 1 ) ,
      sigma_V ~ exponential( 1 ) 
    ) , data = Model_M_list_pre , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MG_pre <- precis( Model_MG_pre , depth = 3 , prob = 0.90 , 
                            pars = c( "bMG" , "bA" , "bR" , "V" ) )
Pre_model_MG_pre

###1.3.2 The number of being nominated for financial provision ----
Model_M_list_fin <- with( BN.data , list(
  Nominated = as.integer( No.be.nominated.financial ) ,
  VID = as.integer( VID ) , 
  Gender = as.integer( Gender ) ,
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) , 
  Pray = as.integer( Pray ) ) )

c( mean = mean( BN.data$No.be.nominated.financial ) ,
   var = var( BN.data$No.be.nominated.financial ) , 
   ratio = var( BN.data$No.be.nominated.financial )/mean( BN.data$No.be.nominated.financial ) ) # Sanity checks: over dispersion

{set.seed(123)
  Model_MG_fin <- ulam(
    alist(
      Nominated ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bMG[Kidnap,Gender] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts  
      
      matrix[Kidnap,Gender]: bMG ~ normal( -0.5 , 0.5 ) ,
      
      phi ~ exponential( 1 ) , 
      
      bA[Age] ~ normal( 0 , 0.5 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 0.2 , 0.3 ) ,
      z_V[VID] ~ normal( 0 , 1 ) ,
      sigma_V ~ exponential( 1 )  
    ) , data = Model_M_list_fin , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MG_fin <- precis( Model_MG_fin , depth = 3 , prob = 0.90 , 
                            pars = c( "bMG" , "bA" , "bR" , "V" ) )
Pre_model_MG_fin

###1.3.3 The number of being nominated for friendships ----
Model_M_list_fri <- with( BN.data , list(
  Nominated = as.integer( No.be.nominated.friend ) ,
  VID = as.integer( VID ) , 
  Gender = as.integer( Gender ) ,
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) , 
  Pray = as.integer( Pray ) ) )

c( mean = mean( BN.data$No.be.nominated.friend ) ,
   var = var( BN.data$No.be.nominated.friend ) , 
   ratio = var( BN.data$No.be.nominated.friend )/mean( BN.data$No.be.nominated.friend ) ) 

{set.seed(123)
  Model_MG_fri <- ulam(
    alist(
      Nominated ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bMG[Kidnap,Gender] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts  
      
      matrix[Kidnap,Gender]: bMG ~ normal( 0 , 0.2 ) ,
      
      phi ~ exponential( 1 ) ,
      
      bA[Age] ~ normal( 0 , 0.5 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 0.4 , 0.3 ) ,
      z_V[VID] ~ normal( 0 , 1 ) ,
      sigma_V ~ exponential( 1 )  
    ) , data = Model_M_list_fri , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MG_fri <- precis( Model_MG_fri , depth = 3 , prob = 0.90 , 
                            pars = c( "bMG" , "bA" , "bR" , "V" ) )
Pre_model_MG_fri

##1.4 Figures ----
Post_model_MG_pre <- extract.samples( Model_MG_pre )
Post_model_MG_fin <- extract.samples( Model_MG_fin )
Post_model_MG_fri <- extract.samples( Model_MG_fri )

###1.4.1 Posterior distributions of estimates ----
{
  par( mfrow = c( 3 , 2 ) , oma = c( 3.5 , 3.5 , 0 , 0 ) , mar = c( 0.3 , 0.3 , 0 , 0 ) )
  # Prestige
  dens( Post_model_MG_pre$bMG[,1,1] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MG_pre$bMG[,2,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Prestige" , side = 2 , line = 2.5 , cex = 1 )
  legend( x = -0.3 , y = 2.6 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" , 
                      "Non-kidnapping male" , 
                      "Kidnapping male" ) , 
          lty = c( 3 , 1 , 3 , 1 ) ,  
          col = c( "#df562c" , "#df562c" , "#038766" , "#038766" ) , 
          lwd = 2 ,
          cex = 1.1 , 
          bty = "n" ,
          y.intersp = 0.8 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
  
  dens( Post_model_MG_pre$bMG[,1,2] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MG_pre$bMG[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  
  # Financial provision
  dens( Post_model_MG_fin$bMG[,1,1] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MG_fin$bMG[,2,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Financial provision" , side = 2 , line = 2.5 , cex = 1 )
  
  dens( Post_model_MG_fin$bMG[,1,2] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MG_fin$bMG[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  
  # Friendships
  dens( Post_model_MG_fri$bMG[,1,1] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MG_fri$bMG[,2,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Friendship" , side = 2 , line = 2.5 , cex = 1 )
  mtext( "Females" , side = 1 , line = 2.5 , cex = 1 )
  
  dens( Post_model_MG_fri$bMG[,1,2] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MG_fri$bMG[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  mtext( "Males" , side = 1 , line = 2.5 , cex = 1 )
}

dev.off()

###1.4.2 Estimate difference of kidnapping and non-kidnapping ----
tibble( Value = c( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] , 
                   Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] , 
                   
                   Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] , 
                   Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] ,
                   
                   Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] , 
                   Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] ) , 
        Type = c( rep( "Prestige" , 12000 ) ,
                  rep( "Financial provision" , 12000 ) ,
                  rep( "Friendship" , 12000 ) ) ,
        Gender = c( rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) , 
                    rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) , 
                    rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "Friendship" , "Financial provision" , "Prestige" ) , 
                         labels = c( "Friendship" , "Financial provision" , "Prestige" ) ) , 
          Gender = factor( Gender , 
                           levels = c( "Male" , "Female" ) , 
                           labels = c( "Male" , "Female" ) ) ) %>% 
  ggplot( aes( x = Value , y = Type , 
               fill = Gender , color = Gender) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = 0.90 , 
                point_interval = mean_qi ,
                normalize = "groups", 
                height = 1 , 
                position = position_dodge( width = 0.4 ) ) + 
  scale_fill_manual( values = scales::alpha( c( "#038766" , "#df562c" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#038766" , "#df562c" ) ) +
  xlab( "Posterior distribution of estimate differences" ) +
  ylab( "Models" ) +
  coord_cartesian( ylim = c( 1.2, 3.5 ) ) +
  scale_x_continuous( limits = c( -1 , 1.5 ) , breaks = c( -1 , 0 , 1 ) ) +
  theme(strip.background = element_rect( color = "black" , fill = "white" ) ,
        strip.text.x = element_text( size = 16 ) ,
        axis.line = element_line( colour = "black" ) ,
        panel.grid.major = element_line( colour = "Gainsboro" ) ,
        panel.grid.minor = element_blank() ,
        panel.border = element_rect( colour = "black" , fill = NA ) ,
        panel.background = element_blank() ,
        plot.title = element_text( size = 16 ) ,
        legend.position = c( 0.99 , 0.99 ) ,
        legend.justification = c( 0.99 , 0.99 ) , 
        axis.title.x = element_text( size = 18 ,
                                     margin = margin( t = 0.3 , r = 0 , b = 0 , l = 0 , unit = "cm" ) ) ,
        axis.text.x = element_text( colour = "black" , size = 16 ,
                                    margin = margin( t = 0.1 , r = 0 , b = 0 , l = 0 , unit = "cm" ) ) ,
        axis.title.y = element_text( size = 18 ,
                                     margin = margin( t = 0 , r = 0.5 , b = 0 , l = 0 , unit = "cm" ) ) ,
        axis.text.y = element_text( colour = "black" , size = 16 ,
                                    margin = margin( t = 0 , r = 0.2 , b = 0 , l = 0 , unit = "cm" ) ) ,
        legend.title=element_text( size = 14 ) ,
        legend.text=element_text( size = 14 ) )


ggsave( filename = "BN_bMG_diff_three_types.jpeg" , 
        width = 180 , height = 120 , units = "mm" , dpi = 300 )

##1.5 Outputs ----
###1.5.1 Estimates ----
Pre_MG_BN_output <- data.frame( Mean = c( Pre_model_MG_pre$mean , 
                                          Pre_model_MG_fin$mean ,
                                          Pre_model_MG_fri$mean ) ,
                                CI5 = c( Pre_model_MG_pre$`5%` ,
                                         Pre_model_MG_fin$`5%` ,
                                         Pre_model_MG_fri$`5%` ) ,
                                CI95 = c( Pre_model_MG_pre$`95%` ,
                                          Pre_model_MG_fin$`95%` ,
                                          Pre_model_MG_fri$`95%` ) ,
                                Type = rep( c( "Prestige" , "Financial provision" , "Friendship" ) , each = 12 ) ,
                                Variable = rep( c( "Non-kidnapped female" , 
                                                   "Kidnapped female" , 
                                                   "Non-kidnapping male" , 
                                                   "Kidnapping male" , 
                                                   "Age: <40" , "Age: 40-49" , 
                                                   "Age: 50-59" , "Age: >=60" , 
                                                   "Pray: seldom" , "Pray: often" , 
                                                   "Village 1" , "Village 2" ) , 3 ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "Prestige" , "Financial provision" , "Friendship" ) ,
                         labels = c( "Prestige" , "Financial provision" , "Friendship" ) ) ,
          Variable = factor( Variable , 
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
          Model = "Social relationship nomination" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

MG_all_output <- tibble( Variable =  c( "Non-kidnapped female" , 
                                        "Kidnapped female" , 
                                        "Non-kidnapping male" , 
                                        "Kidnapping male" , 
                                        "Age: <40" , "Age: 40-49" , 
                                        "Age: 50-59" , "Age: >=60" , 
                                        "Pray: seldom" , "Pray: often" ,
                                        "Village 1" , "Village 2" ) ,
                         Type = "Prestige" ) %>% 
  left_join( Pre_MG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Prestige` = "Mean [CI]" ) %>% 
  mutate( Type = "Financial provision" ) %>% 
  left_join( Pre_MG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Financial provision` = "Mean [CI]" ) %>% 
  mutate( Type = "Friendship" ) %>% 
  left_join( Pre_MG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Friendship` = "Mean [CI]" ) %>% 
  select( -Type ) %>% 
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
              columns = c( "Prestige" , "Financial provision" , "Friendship" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 150 ) )

MG_all_output

###1.5.2 Estimate differences ----
tibble( Type = c( rep( "Prestige" , 2 ) ,
                  rep( "Financial provision" , 2 ) ,
                  rep( "Friendship"  , 2 ) ) ,
        Gender = c( rep( c( "Woman" , "Man" ) , 3 ) ) ,
        bM_diff = c( sprintf("%0.2f" , mean( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] ) ) , 
                     sprintf("%0.2f" , mean( Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] ) ) , 
                     
                     sprintf("%0.2f" , mean( Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] ) ) , 
                     sprintf("%0.2f" , mean( Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] ) ) , 
                     
                     sprintf("%0.2f" , mean( Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] ) ) , 
                     sprintf("%0.2f" , mean( Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] ) ) ) ,
        
        bM_diff_L = c( sprintf("%0.2f" , PI( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] , prob = 0.90 )[1] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] , prob = 0.90 )[1] ) , 
                       
                       sprintf("%0.2f" , PI( Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] , prob = 0.90 )[1] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] , prob = 0.90 )[1] ) , 
                       
                       sprintf("%0.2f" , PI( Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] , prob = 0.90 )[1] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] , prob = 0.90 )[1] ) ) ,
        
        bM_diff_H = c( sprintf("%0.2f" , PI( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] , prob = 0.90 )[2] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] , prob = 0.90 )[2] ) , 
                       
                       sprintf("%0.2f" , PI( Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] , prob = 0.90 )[2] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] , prob = 0.90 )[2] ) , 
                       
                       sprintf("%0.2f" , PI( Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] , prob = 0.90 )[2] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] , prob = 0.90 )[2] ) ) ,
        `bM_diff[90%CI]` = paste( bM_diff , "[" , bM_diff_L , ", " , bM_diff_H , "]") , 
        Pro_bM_diff = c( sprintf( "%0.4f" , length( which( ( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] ) > 0 ) ) / 6000 ) , 
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] ) > 0 ) ) / 6000 ) ,
                         
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] ) > 0 ) ) / 6000 ) , 
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] ) > 0 ) ) / 6000 ) ,
                         
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] ) > 0 ) ) / 6000 ) , 
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bM_diff = paste( round( as.numeric( Pro_bM_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( Type , Gender , `bM_diff[90%CI]` , Pro_bM_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Type , Gender , `bM_diff[90%CI]` , Pro_bM_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )
