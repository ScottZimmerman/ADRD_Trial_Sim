function(inputs){
  library("ggplot2")
  library("tidyr")
  library("dplyr")
  inputs$df_pred %>%
    arrange(months) %>%
    ggplot(aes(months, mem_change, group = Study.Arm,  color = Study.Arm)) +
    geom_line(aes(linetype = Study.Arm), size = 1) +
    geom_point(size = 2, position=position_dodge2(width = 3)) +
    geom_pointrange(aes(ymin = mem_change - sdev ,
                        ymax = mem_change + sdev),
                    width=.2,position=position_dodge2(width =4)) +
    scale_color_manual(values =c("Intensive" = "orange",
                                 "Standard" = "dark gray")) +
    ggtitle("Predicted Memory Trajectories") +
    ylab("Memory Change (Z-Score)") +
    xlab("Time (Months)") +
    theme_minimal() +
    theme(axis.line = element_line(colour = "grey50", size = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line( size=.1, color="black" ),
          panel.grid.minor = element_blank())
}