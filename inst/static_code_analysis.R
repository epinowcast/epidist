library(CodeDepends)

gg <- CodeDepends::makeCallGraph("package:epidist")
gg <- layoutGraph(gg, layoutType = "neato")
graph.par(list(nodes = list(fontsize = 40)))

renderGraph(gg)
