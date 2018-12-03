
produceTable <- function(rows, columns, data) {

}


library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb, "test-1")ß

createRow <- function(x) {

}

mergeCells(wb = wb, sheet = "test-1", cols = 1:3, rows = 1)

writeData(wb = wb, sheet = "test-1", x = c("Car"))
wb$sharedStrings[[1]] <- "<si><r><rPr><i/></rPr><t xml:space=\"preserve\">Ca</t></r><r><rPr><b/></rPr><t xml:space=\"preserve\">r</t></r></si>"



openXL(wb)

fake_set <- function() {
  data.frame(
    Condition = sample(0:2, size = 100, replace = T),
    DV1 = rnorm(100, 1, 1.25),
    DV2 = rnorm(100, 2, 1.25),
    DV3 = rnorm(100, 3, 1.25),
    DV4 = rnorm(100, 4, 1.25)
    )
}

Study1 <- fake_set()
Study2 <- fake_set()

FullSet <- rbind(data.frame(Study1, Study = "S1"), data.frame(Study2, Study = "S2"))

FullSet_aggr <- aggregate(x = FullSet[!colnames(FullSet) %in% c("Condition", "Study")], by = list(Study = FullSet$Study, Condition = FullSet$Condition),
          FUN = mean)

FullSet_melt <- reshape2::melt(FullSet_aggr, id=c("Study","Condition"))
FullSet_melt[["DV"]] <- FullSet_melt$variable
FullSet_melt$variable <- NULL
FullSet_melt$DV_cat <- sapply(FullSet_melt$DV, function(x){
  if(x == "DV1") {
    "A"
  } else if(x == "DV2") {
    "A"
  } else {
    "B"
  }
})

FS <- FullSet_melt

desc1 <- psych::describeBy(Study1[-1], group = Study1$Condition)


desc1b <- Reduce(f = function(endproduct, name){
  d <- as.data.frame(desc1[[name]])[,c("mean", "se")]
  d[["DV"]] <- rownames(d)
  d[["Condition"]] <- name
  rownames(d) <- NULL
  rbind(endproduct, d)
}, x = names(desc1), init = NULL)

desc1b$DV_cat <- sapply(X = desc1b$DV, FUN = function(x){
  if(x == "DV1") {
    "A"
  } else if(x == "DV2") {
    "A"
  } else if(x == "DV3") {
    "B"
  } else if(x == "DV4") {
    "B"
  }
})

desc1b <- lapply(names(desc1), function(name){
  desc1[[name]][["Condition"]] <- name
  as.data.frame(desc1[[name]])
})
melt1 <- reshape2::melt(desc1b)
