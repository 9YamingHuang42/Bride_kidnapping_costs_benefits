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

#3 Fertility ----
##3.1 DAG ----
Dag_RS <- 
  dagitty( 'dag {
bb="-0.5,-0.5,0.5,0.5"
Age [pos="-0.150,-0.330"]
Brideprice [pos="-0.200,0.100"]
Cur.wealth [pos="0.200,0.100"]
Fertility [outcome,pos="0.000,0.200"]
His.wealth [latent,pos="-0.270,-0.160"]
Marriage [exposure,pos="0.150,-0.330"]
Religiosity [pos="0.270,-0.160"]
U_village [latent,pos="0.120,0.200"]
Age -> Brideprice
Age -> Fertility
Age -> His.wealth
Age -> Marriage
Age -> Religiosity
Brideprice -> Cur.wealth
His.wealth -> Brideprice
His.wealth -> Cur.wealth
His.wealth -> Fertility
Marriage -> Brideprice
Marriage -> Fertility
Religiosity -> Fertility
Religiosity -> Marriage
U_village -> Fertility
}')

adjustmentSets( Dag_RS , 
                exposure = "Marriage" , 
                outcome = "Fertility" ,
                effect = "total" )

##3.2 Data preparation ----
load( file = "Bride_kidnapping_fertility_analyses.RData" )

##3.3 Models ----
Model_list_RS <- with( RS.data , list(
  Fertility = No.children ,
  VID = as.integer( VID ) , 
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) , 
  Pray = as.integer( Pray ) ) )

{set.seed(123)
  Model_M_RS <- ulam(
    alist(
      Fertility ~ normal( mu , sigma ),
      mu <- bM[Kidnap] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts 
      
      bM[Kidnap] ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.6 ) ,
      bR[Pray] ~ normal( 0 , 0.3 ) ,
      
      sigma ~ exponential( 1 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) 
    ) , data = Model_list_RS , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_M_RS <- precis( Model_M_RS , depth = 3 , prob = 0.90 , 
                          pars = c( "bM" , "bA" , "bR" , "V" ) )
Pre_model_M_RS

##3.4 Figures ----
Post_model_M_RS <- extract.samples( Model_M_RS )

###3.4.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 1 , 1 ) , oma = c( 1.2 , 1.2 , 0.2 , 0.2 ) , mar = c( 0 , 0 , 0 , 0 ) )
  dens( Post_model_M_RS$bM[,1] , 
        adj = 1 , 
        xlim = c( -1.8 , 1.8 ) ,
        ylim = c( 0 , 1.6 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#e47178" , cex.axis = 1.2 )
  dens( Post_model_M_RS$bM[,2] ,
        adj = 1 , 
        lwd = 3 , col = "#e47178" , 
        add = T )
  mtext( "Density" , side = 2 , line = 2.2 , cex = 1.5 )
  mtext( "Posterior distribution of estimates" , side = 1 , line = 2.4 , cex = 1.5 )
  
  legend( x = -0.1 , y = 1.7 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" ) , 
          lty = c( 3 , 1 ) ,  
          col = c( "#e47178" , "#e47178" ) , 
          lwd = 2 ,
          cex = 1 , 
          bty = "n" ,
          y.intersp = 1.2 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
}

dev.off()

###3.4.2 Estimate difference of kidnapping and non-kidnapping (bride-price + marriage age + fertility) ----
tibble( Value = c( Post_M_LS$bM[,2] - Post_M_LS$bM[,1] , 
                   Post_M_C$bM[,2] - Post_M_C$bM[,1] , 
                   Post_M_Com$bM[,2] - Post_M_Com$bM[,1] , 
                   
                   Post_model_MG_MA$bMG[,2,1] - Post_model_MG_MA$bMG[,1,1] , 
                   Post_model_MG_MA$bMG[,2,2] - Post_model_MG_MA$bMG[,1,2] ,
                   
                   Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] ) ,
        Group = c( rep( "Couple" , 6000 ) , 
                   rep( "Couple" , 6000 ) , 
                   rep( "Couple" , 6000 ) , 
                   
                   rep( "Female" , 6000 ) ,
                   rep( "Male" , 6000 ) , 
                   
                   rep( "Couple" , 6000 ) ) , 
        Model = c( rep( "Livestock (BP)" , 6000 ) ,
                   rep( "Money (BP)" , 6000 ) , 
                   rep( "Combined (BP)" , 6000 ) , 
                   
                   rep( "Marriage age" , 6000 ) ,
                   rep( "Marriage age" , 6000 ) ,
                   
                   rep( "Fertility" , 6000 ) ) ) %>% 
  mutate( Group = factor( Group , 
                          levels = c( "Couple" , "Male" , "Female" ) , 
                          labels = c( "Couple" , "Male" , "Female" ) ) , 
          Model = factor( Model , 
                          levels = c( "Fertility" , "Marriage age" , 
                                      "Combined (BP)" , "Livestock (BP)" , "Money (BP)" ) , 
                          labels = c( "Fertility" , "Marriage age" , 
                                      "Combined (BP)" , "Livestock (BP)" , "Money (BP)" ) ) ) %>% 
  ggplot( aes( x = Value , y = Model , 
               fill = Group , color = Group) ) +
  geom_vline( xintercept = 0 , linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = 0.90 , 
                point_interval = mean_qi ,
                normalize = "groups", 
                height = 1.4 , 
                position = position_dodge( width = 0.6 ) ) + 
  scale_fill_manual( values = scales::alpha( c( "#040676" , "#a0add0" , "#e47178" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#040676" , "#a0add0" , "#e47178" ) ) +
  xlab( "Posterior distribution of estimate differences" ) +
  ylab( "Models" ) +
  coord_cartesian( ylim = c( 1.3 , 5.5 ) ) +
  scale_x_continuous( limits = c( -2.2 , 1.2 ) , breaks = c( -2 , -1 , 0 , 1 ) ) +
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
        axis.title.x = element_text( size = 16 ,
                                     margin = margin( t = 0.3 , r = 0 , b = 0 , l = 0 , unit = "cm" ) ) ,
        axis.text.x = element_text( colour = "black" , size = 14 ,
                                    margin = margin( t = 0.1 , r = 0 , b = 0 , l = 0 , unit = "cm" ) ) ,
        axis.title.y = element_text( size = 16 ,
                                     margin = margin( t = 0 , r = 0.5 , b = 0 , l = 0 , unit = "cm" ) ) ,
        axis.text.y = element_text( colour = "black" , size = 14 ,
                                    margin = margin( t = 0 , r = 0.2 , b = 0 , l = 0 , unit = "cm" ) ) ,
        legend.title=element_text( size = 12 ) ,
        legend.text=element_text( size = 12 ) )

ggsave( filename = "D:\\Kidnapping\\BP+MA+RS_bM_diff.jpeg" , 
        width = 180 , height = 120 , units = "mm" , dpi = 300 )

##3.5 Outputs ----
###3.5.1 Estimates ----
Pre_M_RS_output <- data.frame( Mean = Pre_model_M_RS$mean ,
                               CI5 = Pre_model_M_RS$`5%` ,
                               CI95 = Pre_model_M_RS$`95%` ,
                               Variable = c( "Non-kidnapped" , 
                                             "Kidnapped" , 
                                             "Age: <40" , "Age: 40-49" , 
                                             "Age: 50-59" , "Age: >=60" , 
                                             "Pray: seldom" , "Pray: often" , 
                                             "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Non-kidnapped" , 
                                         "Kidnapped" , 
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" , 
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Non-kidnapped" , 
                                         "Kidnapped" , 
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" , 
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Fertility" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

M_RS_output <- Pre_M_RS_output %>% 
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

M_RS_output

###3.5.2 Estimate differences ----
tibble( Gender = c( "Woman" ) ,
        bM_diff = c( sprintf("%0.2f" , mean( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] ) ) ) ,
        
        bM_diff_L = c( sprintf("%0.2f" , PI( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] , prob = 0.90 )[1] ) ) ,
        
        bM_diff_H = c( sprintf("%0.2f" , PI( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] , prob = 0.90 )[2] ) ) ,
        `bM_diff[90%CI]` = paste( bM_diff , "[" , bM_diff_L , ", " , bM_diff_H , "]") , 
        Pro_bM_diff = c( sprintf( "%0.4f" , 
                                  length( which( ( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] ) > 0 ) ) / 6000 ) ) ) %>% 
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
