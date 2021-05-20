library(lattice)
library(gridExtra, warn.conflicts = FALSE)

#' Fonction permettant d'ordonner les différentes corrélations par ordre
#' décroissant par défaut en fonction de la valeur absolue de la corrélation
#'
#' @param df le dataframe dont les corrélations sont à extraire
#' @param order paramètre optionnel pour préciser l'ordre de tri (defaut:décroi)
#' @param threshold paramètre optionnel pour préciser un seuil (défaut: <=0.5)
#' @return un tableau contenant les corrélations ordonnées
getOrderedCorrelations <- function(df, order=T, threshold=0.5) {
    orderedCor <- as.data.frame(as.table(df))
    orderedCor <- orderedCor[order(abs(orderedCor$Freq), decreasing=order),]
    orderedCor <- orderedCor[abs(orderedCor$Freq)<1 & abs(orderedCor$Freq)>=threshold,]
    rownames(orderedCor) <- NULL
    withoutDuplicated <- orderedCor[seq(from=1, to=nrow(orderedCor), by=2),]
    rownames(withoutDuplicated) <- NULL
    return(withoutDuplicated)
}

afficher_table_contingence_clusters = function(factor, cluster){
    cat("Valeurs absolues :","\n")
    print(rbind(table(factor, cluster), 
                tot=table(cluster)))
    
    cat("\n","Valeurs relatives :","\n")
    print(rbind(prop.table(table(factor, cluster)), 
                tot=prop.table(table(cluster)))*100)
}

#' Fonction permettant d'afficher une série de strip plots des variables d'un
#' data.frame et de les colorer et fonction de classes.
#' 
#' @param scaledQuantitativeValues data.frame ne comportant que des valeurs 
#' quantitatives normalisées
#' @param cluster classes associées aux individus du data.frame
stripplot_clusters <- function(scaledQuantitativeValues, cluster, fig){
    v <- as.vector(unlist(cars.scaled[-8]))
    y <- rep(c(1:ncol(scaledQuantitativeValues)), each=nrow(scaledQuantitativeValues))
    xydata <- data.frame(v, y)
    colors = rep(cluster, ncol(scaledQuantitativeValues))
    xyplot(y ~ v, 
           xydata, 
           groups=colors,
           main = paste("Fig.",fig,": Distribution des variables de cars colorées par", 
                        length(unique(cluster)),
                        "classes"),
           xlab = "Distributions",
           ylab = "Index des variables de cars",
           auto.key = list(space="top", x=0, y=0),
           lattice.options = list(legend.bbox="panel"))
}

cut_affiche_table_contingence = function(hc_result, cut_vector, factor){
    for(cut in cut_vector){
        cat(paste("\nCut =", cut, "\n"))
        c = cutree(hc_result, cut)
        afficher_table_contingence_clusters(factor, c)
    }
}