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

#4 Bride-price ----
##4.1 DAG ----
Dag_BR <- 
  dagitty( 'dag {
bb="-0.5,-0.5,0.5,0.5"
"Marriage cohort" [pos="-0.265,-0.050"]
"Marriage type" [exposure,pos="0.000,-0.260"]
Brideprice [outcome,pos="0.000,0.320"]
Cur.wealth [pos="0.200,0.170"]
His.wealth [latent,pos="-0.200,0.170"]
Religiosity [pos="0.265,-0.050"]
U_village [latent,pos="0.156,0.352"]
"Marriage cohort" -> "Marriage type"
"Marriage cohort" -> Brideprice
"Marriage cohort" -> His.wealth
"Marriage cohort" -> Religiosity
"Marriage type" -> Brideprice
Brideprice -> Cur.wealth
His.wealth -> Brideprice
His.wealth -> Cur.wealth
Religiosity -> "Marriage type"
U_village -> Brideprice
}
')

adjustmentSets( Dag_BR , 
                exposure = "Marriage type" , 
                outcome = "Brideprice" ,
                effect = "total" )

##4.2 Data loading ----
load( file = "Bride_kidnapping_bride_price_analyses.RData" )

##4.3 Models ----
###4.3.1 Livestock ----
Model_list_BP <- with( Data.BP , list(
  VID = as.integer( VID ) , 
  BP_sheep = BP_sheep , 
  BP_USD = BP_USD_k , 
  BP_comb = BP.combined ,
  Kidnap = as.integer( Kidnap ) ,
  Cohort = as.integer( Marriage.year.c ) ) ) 

c( mean = mean( Data.BP$BP_sheep ) ,
   var = var( Data.BP$BP_sheep ) ,
   ratio = var( Data.BP$BP_sheep )/mean( Data.BP$BP_sheep ) ) # Sanity checks: over dispersion 

{set.seed(123)
  Model_M_LS <- ulam(
    alist(
      # a negative-binomial model
      BP_sheep ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bM[Kidnap] + bC[Cohort] + bV[VID] ,
      
      # define village effects using other parameters
      transpars> vector[VID]: bV <<- bVbar + zV*sigmaV ,
      
      # priors
      phi ~ exponential( 1 ) , 
      
      bM[Kidnap] ~ normal( 0 , 0.3 ) , 
      
      bC[Cohort] ~ normal( 0 , 0.5 ) ,
      
      bVbar ~ normal( 0 , 0.3 ) , 
      zV[VID] ~ normal( 0 , 1 ) , 
      sigmaV ~ exponential( 1 ) 
    ) , data = Model_list_BP ,
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 )
  ) }

Pre_model_M_LS <- precis( Model_M_LS , depth = 3 , prob = 0.90 , 
                          pars = c( "bC" , "bM" , "bV" ) )
Pre_model_M_LS

###4.3.2 Cash ----
####4.3.2.1 Marriage type ----
c( mean = mean( Data.BP$BP_USD_k ) ,
   var = var( Data.BP$BP_USD_k ) , 
   ratio = var( Data.BP$BP_USD_k )/mean( Data.BP$BP_USD_k ) ) # Sanity checks: not over dispersion 

{set.seed(123)
  Model_M_C <- ulam(
    alist(
      # a poisson model
      BP_USD ~ dpois( lambda ) ,
      log( lambda ) <- bM[Kidnap] + bC[Cohort] + bV[VID] ,
      
      # define village effects using other parameters
      transpars> vector[VID]: bV <<- bVbar + zV*sigmaV ,
      
      # priors
      bM[Kidnap] ~ normal( 0 , 0.5 ) , 
      
      bC[Cohort] ~ normal( 0 , 0.5 ) ,
      
      bVbar ~ normal( 0 , 0.3 ) , 
      zV[VID] ~ normal( 0 , 1 ) , 
      sigmaV ~ exponential( 1 ) 
    ) , data = Model_list_BP ,
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 )
  ) }

Pre_model_M_C <- precis( Model_M_C , depth = 3 , prob = 0.90 , 
                         pars = c( "bC" , "bM" , "bV" ) )
Pre_model_M_C

###4.3.3 Combined variable ----
# Student-t distribution due to a few large values in the upper tail
{set.seed(123)
  Model_M_Com <- ulam(
    alist(
      # a Student-t model
      BP_comb ~ dstudent( 2 , mu , sigma ) ,
      mu <- bM[Kidnap] + bC[Cohort] + bV[VID] ,
      
      # define village effects using other parameters
      transpars> vector[VID]: bV <<- bVbar + zV*sigmaV ,
      
      # priors
      bM[Kidnap] ~ normal( 0 , 0.5 ) , 
      
      bC[Cohort] ~ normal( 0 , 0.5 ) ,
      
      bVbar ~ normal( 0 , 0.3 ) , 
      zV[VID] ~ normal( 0 , 1 ) , 
      sigmaV ~ exponential( 1 ) ,
      sigma ~ exponential( 1 ) 
    ) , data = Model_list_BP ,
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 )
  ) }

Pre_model_M_Com <- precis( Model_M_Com , depth = 3 , prob = 0.90 , 
                           pars = c( "bC" , "bM" , "bV" ) )
Pre_model_M_Com

##4.4 Figures ----
Post_M_LS <- extract.samples( Model_M_LS )
Post_M_C <- extract.samples( Model_M_C )
Post_M_Com <- extract.samples( Model_M_Com )

###4.4.1 Posterior distribution of estimates ----
tibble( Value = c( Post_M_LS$bM[,1] , 
                   Post_M_LS$bM[,2] , 
                   
                   Post_M_C$bM[,1] , 
                   Post_M_C$bM[,2] , 
                   
                   Post_M_Com$bM[,1] , 
                   Post_M_Com$bM[,2] ) ,
        Model = c( rep( "Livestock" , 12000 ) ,
                   rep( "Money" , 12000 ) , 
                   rep( "Combined" , 12000 ) ) , 
        Marriage = rep( c( rep( "Non-kidnap" , 6000 ) ,
                           rep( "Kidnap" , 6000 ) ) , 3  ) ) %>% 
  mutate( Model = factor( Model , 
                          levels = c( "Combined" , "Livestock" , "Money" ) , 
                          labels = c( "Combined" , "Livestock" , "Money"  ) ) , 
          Marriage = factor( Marriage , 
                             levels = c( "Kidnap" , "Non-kidnap" ) , 
                             labels = c( "Kidnap" , "Non-kidnap" ) ) ) %>% 
  ggplot( aes( x = Value, y = Model , 
               fill = Marriage , 
               color = Marriage ) ) +
  geom_vline( xintercept = 0 , 
              linetype = 2 , 
              linewidth = 0.8 , 
              color = "grey40" ) +
  stat_halfeye( .width = 0.90 , 
                point_interval = mean_qi ,
                normalize = "groups", 
                height = 1 , 
                position = position_dodge( width = 0.6 ) ) +
  scale_fill_manual( values = scales::alpha( c( "#040676" , "#397FC7" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#040676" , "#397FC7" ) ) +
  labs( x = "Posterior distribution of estimates" ,
        y = "Model" ) +
  coord_cartesian( ylim = c( 1.2, 3.3 ) ) +
  scale_x_continuous( limits = c( -2.5 , 2.5 ) , breaks = c( -2 , -1 , 0 , 1 , 2 ) ) +
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
        legend.text=element_text( size = 12 ) )

ggsave( filename = "BP_bM.jpeg" , 
        width = 180 , height = 120 , units = "mm" , dpi = 300 )

##4.5 Outputs ----
###4.5.1 Estimates ----
####4.5.1.1 Livestock ----
M_LS_output <- data.frame( Mean = c( Pre_model_M_LS$mean ) ,
                           CI5 = c( Pre_model_M_LS$`5%` ) ,
                           CI95 = c( Pre_model_M_LS$`95%` ) ,
                           Variable = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                         "1991-2000" , "2001-2010" , ">2010" ,
                                         
                                         "Non-kidnap" , "Kidnap" ,
                                         
                                         "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                         "1991-2000" , "2001-2010" , ">2010" ,
                                         
                                         "Non-kidnap" , "Kidnap" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                         "1991-2000" , "2001-2010" , ">2010" ,
                                         
                                         "Non-kidnap" , "Kidnap" ,
                                         
                                         "Village 1" , "Village 2" ) ) ,
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

M_LS_output[,c(4:5)] %>% 
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
  cols_align( align = "center" ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 150 ) )

####4.5.1.2 Cash ----
M_C_output <- data.frame( Mean = c( Pre_model_M_C$mean ) ,
                          CI5 = c( Pre_model_M_C$`5%` ) ,
                          CI95 = c( Pre_model_M_C$`95%` ) ,
                          Variable = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                        "1991-2000" , "2001-2010" , ">2010" ,
                                        
                                        "Non-kidnap" , "Kidnap" ,
                                        
                                        "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                         "1991-2000" , "2001-2010" , ">2010" ,
                                         
                                         "Non-kidnap" , "Kidnap" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                         "1991-2000" , "2001-2010" , ">2010" ,
                                         
                                         "Non-kidnap" , "Kidnap" ,
                                         
                                         "Village 1" , "Village 2" ) ) ,
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

M_C_output[,c(4:5)] %>% 
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
  cols_align( align = "center" ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 150 ) )

####4.5.1.3 Combined ----
M_Com_output <- data.frame( Mean = c( Pre_model_M_Com$mean ) ,
                            CI5 = c( Pre_model_M_Com$`5%` ) ,
                            CI95 = c( Pre_model_M_Com$`95%` ) ,
                            Variable = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                          "1991-2000" , "2001-2010" , ">2010" ,
                                          
                                          "Non-kidnap" , "Kidnap" ,
                                          
                                          "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                         "1991-2000" , "2001-2010" , ">2010" ,
                                         
                                         "Non-kidnap" , "Kidnap" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "<=1970" , "1971-1980" , "1981-1990" ,
                                         "1991-2000" , "2001-2010" , ">2010" ,
                                         
                                         "Non-kidnap" , "Kidnap" ,
                                         
                                         "Village 1" , "Village 2" ) ) ,
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

M_Com_output[,c(4:5)] %>% 
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
  cols_align( align = "center" ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 150 ) )

###4.5.2 Estimate differences ----
tibble( Model = c( "Livestock" , "Cash" , "Combined" ) ,
        bM_diff = c( sprintf("%0.2f" , mean( Post_M_LS$bM[,2] - Post_M_LS$bM[,1] ) ) ,
                     
                     sprintf("%0.2f" , mean( Post_M_C$bM[,2] - Post_M_C$bM[,1] ) ) ,  
                     
                     sprintf("%0.2f" , mean( Post_M_Com$bM[,2] - Post_M_Com$bM[,1] ) ) ) ,
        
        bM_diff_L = c( sprintf("%0.2f" , PI( Post_M_LS$bM[,2] - Post_M_LS$bM[,1] , prob = 0.90 )[1] ) , 
                       
                       sprintf("%0.2f" , PI( Post_M_C$bM[,2] - Post_M_C$bM[,1] , prob = 0.90 )[1] ) , 
                       
                       sprintf("%0.2f" , PI( Post_M_Com$bM[,2] - Post_M_Com$bM[,1] , prob = 0.90 )[1] ) ) ,
        
        bM_diff_H = c( sprintf("%0.2f" , PI( Post_M_LS$bM[,2] - Post_M_LS$bM[,1] , prob = 0.90 )[2] ) , 
                       
                       sprintf("%0.2f" , PI( Post_M_C$bM[,2] - Post_M_C$bM[,1] , prob = 0.90 )[2] ) , 
                       
                       sprintf("%0.2f" , PI( Post_M_Com$bM[,2] - Post_M_Com$bM[,1] , prob = 0.90 )[2] ) ) ,
        "bM_diff[90%CI]" = paste( bM_diff , "[" , bM_diff_L , ", " , bM_diff_H , "]") , 
        Pro_bM_diff = c( sprintf( "%0.4f" , length( which( ( Post_M_LS$bM[,2] - Post_M_LS$bM[,1] ) > 0 ) ) / 6000 ) , 
                         
                         sprintf( "%0.4f" , length( which( ( Post_M_C$bM[,2] - Post_M_C$bM[,1] ) > 0 ) ) / 6000 ) , 
                         
                         sprintf( "%0.4f" , length( which( ( Post_M_Com$bM[,2] - Post_M_Com$bM[,1] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bM_diff = paste( round( as.numeric( Pro_bM_diff ) * 100 , 2 ) , "%" ) ) %>% 
  dplyr::select( Model , "bM_diff[90%CI]" , Pro_bM_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Model , "bM_diff[90%CI]" , Pro_bM_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )