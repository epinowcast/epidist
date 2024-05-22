library(CodeDepends)

gg <- CodeDepends::makeCallGraph("package:epidist")
gg <- layoutGraph(gg, layoutType = "neato")
graph.par(list(nodes = list(fontsize = 40)))

png(filename = "inst/code_depends.png", width = 5, height = 5, units = "in", res = 300)
renderGraph(gg)
dev.off()
