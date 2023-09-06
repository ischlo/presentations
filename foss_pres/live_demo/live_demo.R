# live demo

library(rlist)
library(cppSim)
library(data.table)

source('test_env/run_network_sim.R')
####

getwd()

distance_matrices <- rlist::list.load('test_env/distance_matrices.rds')

flows_mat <- rlist::list.load('test_env/flow_mat/flows_mat.rds')

# lapply(distance_matrices, FUN = \(mat) which(mat))

#### running sim 

sim <- mapply(names(distance_matrices),distance_matrices,SIMPLIFY = FALSE, FUN = \(name,dist) {
  n <- strsplit(name,'_') |> unlist()
  # print(name)
  if(n[2]=='all') {
    
    res_1 <- run_network_sim(dist_mat = dist
                             ,flows_mat = flows_mat[['at']]
                             ,beta_init = .4
                             ,step = .03)
    
    res <- list('at'=res_1)
    
  } else res <- list()

  return(res)
  
})

str(sim)


# rlist::list.save(sim,'test_env/sim.rds')

results_at <- list("osm_geom" = sim$osm_all_geom$at
                   ,"osm_commute" = sim$osm_all_commute$at
                   ,"osm_net" = sim$osm_all_net$at
                   ,"os_geom" = sim$os_all_geom$at
                   ,"os_commute" = sim$os_all_commute$at
                   ,"os_net" = sim$os_all_net$at
                   # ,"norm2_geom" = norm2_geom_results
                   # ,"norm2_commute"=norm2_commute_results
                   # ,"norm2_net"=norm2_network_results
)


beta_results_at <- cbind(beta = results_at[[1]]$beta_calib[,1]
                         ,lapply(results_at, FUN = function(res) res$beta_calib[,2]) |> as.data.frame()) |> 
  as.matrix()


{
  coul <- 1:6
  p_types <- c(15,17,18)
  l_type <- c(1:4)
  
  # jpeg("test_env/quality_fit_at.jpg"
  #      ,height = 5.83
  #      ,width = 5.83
  #      ,quality = 80
  #      ,units = "in"
  #      ,res = 150)
  # 
  par(mar = c(4,5,4,1))
  plot(results_at[[1]]$beta_calib[,c(1,2)]
       ,xlim = c(.4,1.4)
       ,ylim = c(.6,.9)
       ,cex = 1
       ,type = "n"
       ,xlab = expression(beta)
       ,ylab = expression(r^2)
       ,cex.lab =1.2)
  
  title("Quality of fit, active travel"
        ,adj = 0
        ,line = 0.5)
  
  legend(x = "bottomleft"
         ,legend = names(results_at)
         ,col = coul
         ,lwd = 2
         ,pch = p_types
         ,lty = l_type)
  
  mapply(coul,p_types,l_type,results_at, FUN = function(col,p_t,l_t,res) lines(res$beta_calib[,c(1,2)]
                                                                               ,col = col
                                                                               ,lwd =2
                                                                               ,type = "b"
                                                                               ,lty = l_t
                                                                               ,pch = p_t))
  # dev.off()
  }

