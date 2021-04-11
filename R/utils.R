


#' getqualcolor
#'
#' Get color list for discrete values
#'
#' @param colors_n colors number
#'
#' @importFrom scales brewer_pal
#' @importFrom wesanderson wes_palette
#' @importFrom randomcoloR distinctColorPalette
#' @importFrom ggsci pal_aaas pal_d3 pal_rickandmorty pal_futurama pal_gsea
#' pal_igv pal_jama pal_jco pal_lancet pal_locuszoom pal_nejm pal_npg
#' pal_simpsons pal_uchicago pal_startrek pal_tron pal_ucscgb
#'
#' @noRd

getqualcolor <- function(colors_n) {
  colors <- data.frame(matrix(c(
    "Set1", paste0(" scales::brewer_pal(palette = 'Set1')(", colors_n, ")"), 9,
    "Set2", paste0(" scales::brewer_pal(palette = 'Set2')(", colors_n, ")"), 8,
    "Set3", paste0(" scales::brewer_pal(palette = 'Set3')(", colors_n, ")"), 12,
    "Paired", paste0(" scales::brewer_pal(palette = 'Paired')(", colors_n, ")"), 8,
    "Pastel1", paste0(" scales::brewer_pal(palette = 'Pastel1')(", colors_n, ")"), 9,
    "Pastel2", paste0(" scales::brewer_pal(palette = 'Pastel2')(", colors_n, ")"), 8,
    "Dark2", paste0(" scales::brewer_pal(palette = 'Dark2')(", colors_n, ")"), 8,
    "Accent", paste0(" scales::brewer_pal(palette = 'Accent')(", colors_n, ")"), 8,
    "random", paste0(" randomcoloR::distinctColorPalette(", colors_n, ")"), colors_n,
    #"rainbow", paste0(" substr(rainbow(", colors_n, "),1,7)"), colors_n,
    "BottleRocket1", paste0("wesanderson::wes_palette('BottleRocket1',n=", colors_n, ")"), 7,
    "BottleRocket2", paste0("wesanderson::wes_palette('BottleRocket2',n=", colors_n, ")"), 5,
    "Rushmore1", paste0("wesanderson::wes_palette('Rushmore1',n=", colors_n, ")"), 5,
    "Royal1", paste0("wesanderson::wes_palette('Royal1',n=", colors_n, ")"), 4,
    "Royal2", paste0("wesanderson::wes_palette('Royal2',n=", colors_n, ")"), 5,
    "Zissou1", paste0("wesanderson::wes_palette('Zissou1',n=", colors_n, ")"), 5,
    "Darjeeling1", paste0("wesanderson::wes_palette('Darjeeling1',n=", colors_n, ")"), 5,
    "Darjeeling2", paste0("wesanderson::wes_palette('Darjeeling2',n=", colors_n, ")"), 5,
    "Chevalier1", paste0("wesanderson::wes_palette('Chevalier1',n=", colors_n, ")"), 4,
    "FantasticFox1", paste0("wesanderson::wes_palette('FantasticFox1',n=", colors_n, ")"), 5,
    "Moonrise1", paste0("wesanderson::wes_palette('Moonrise1',n=", colors_n, ")"), 4,
    "Moonrise2", paste0("wesanderson::wes_palette('Moonrise2',n=", colors_n, ")"), 4,
    "Moonrise3", paste0("wesanderson::wes_palette('Moonrise3',n=", colors_n, ")"), 5,
    "Cavalcanti1", paste0("wesanderson::wes_palette('Cavalcanti1',n=", colors_n, ")"), 5,
    "GrandBudapest1", paste0("wesanderson::wes_palette('GrandBudapest1',n=", colors_n, ")"), 4,
    "GrandBudapest2", paste0("wesanderson::wes_palette('GrandBudapest2',n=", colors_n, ")"), 4,
    "IsleofDogs1", paste0("wesanderson::wes_palette('IsleofDogs1',n=", colors_n, ")"), 6,
    "IsleofDogs2", paste0("wesanderson::wes_palette('IsleofDogs2',n=", colors_n, ")"), 5,
    "aaas", paste0("substr(ggsci::pal_aaas('default')(", colors_n, "),1,7)"), 10,
    "d3", paste0("substr(ggsci::pal_d3('category10')(", colors_n, "),1,7)"), 10,
    "futurama", paste0("substr(ggsci::pal_futurama('planetexpress')(", colors_n, "),1,7)"), 12,
    "gsea", paste0("substr(ggsci::pal_gsea('default')(", colors_n, "),1,7)"), 12,
    "igv", paste0("substr(ggsci::pal_igv('default')(", colors_n, "),1,7)"), 51,
    "igv2", paste0("substr(ggsci::pal_igv('alternating')(", colors_n, "),1,7)"), 2,
    "jama", paste0("substr(ggsci::pal_jama('default')(", colors_n, "),1,7)"), 7,
    "jco", paste0("substr(ggsci::pal_jco('default')(", colors_n, "),1,7)"), 10,
    "lancet", paste0("substr(ggsci::pal_lancet('lanonc')(", colors_n, "),1,7)"), 9,
    "locuszoom", paste0("substr(ggsci::pal_locuszoom('default')(", colors_n, "),1,7)"), 7,
    "nejm", paste0("substr(ggsci::pal_nejm('default')(", colors_n, "),1,7)"), 8,
    "nrc", paste0("substr(ggsci::pal_npg('nrc')(", colors_n, "),1,7)"), 10,
    "schwifty", paste0("substr(ggsci::pal_rickandmorty('schwifty')(", colors_n, "),1,7)"), 12,
    "simpsons", paste0("substr(ggsci::pal_simpsons('springfield')(", colors_n, "),1,7)"), 16,
    "uchicago", paste0("substr(ggsci::pal_uchicago('default')(", colors_n, "),1,7)"), 9,
    "startrek", paste0("substr(ggsci::pal_startrek('uniform')(", colors_n, "),1,7)"), 7,
    "tron", paste0("substr(ggsci::pal_tron('legacy')(", colors_n, "),1,7)"), 7,
    "ucscgb", paste0("substr(ggsci::pal_ucscgb('default')(", colors_n, "),1,7)"), 26), byrow = T, ncol = 3),
    stringsAsFactors = F)
  colors$X3 <- as.numeric(colors$X3)
  colors_list <- as.list(subset(colors, X3 >= colors_n)$X2)
  names(colors_list) <- subset(colors, X3 >= colors_n)$X1
  colors_list <- lapply(colors_list, function(x) {
    eval(parse(text = x))
  })
  return(colors_list)
}


#' getseqcolor
#'
#' Get color list for continuous values
#'
#' @importFrom scales brewer_pal
#' @importFrom viridis viridis magma inferno plasma cividis
#'
#' @noRd

getseqcolor <- function(){
  palette <- list(
    'YlOrRd' = scales::brewer_pal(palette = 'YlOrRd')(9),
    'YlOrBr' = scales::brewer_pal(palette = 'YlOrBr')(9),
    'YlGnBu'=scales::brewer_pal(palette = 'YlGnBu')(9),
    'YlGn' = scales::brewer_pal(palette = 'YlGn')(9),
    'Reds' = scales::brewer_pal(palette = 'Reds')(9),
    'RdPu' = scales::brewer_pal(palette = 'RdPu')(9),
    'Purples' = scales::brewer_pal(palette = 'Purples')(9),
    'PuRd' = scales::brewer_pal(palette = 'PuRd')(9),
    'PuBuGn' = scales::brewer_pal(palette = 'PuBuGn')(9),
    'PuBu' = scales::brewer_pal(palette = 'PuBu')(9),
    'OrRd' = scales::brewer_pal(palette = 'OrRd')(9),
    'Oranges' = scales::brewer_pal(palette = 'Oranges')(9),
    'Greys' = scales::brewer_pal(palette = 'Greys')(9),
    'Greens' = scales::brewer_pal(palette = 'Greens')(9),
    'GnBu' = scales::brewer_pal(palette = 'GnBu')(9),
    'BuPu' = scales::brewer_pal(palette = 'BuPu')(9),
    'BuGn' = scales::brewer_pal(palette = 'BuGn')(9),
    'Blues' = scales::brewer_pal(palette = 'Blues')(9),
    'Spectral' = scales::brewer_pal(palette = 'Spectral')(9),
    'RdYlGn' = scales::brewer_pal(palette = 'RdYlGn')(9),
    'RdYlBu' = scales::brewer_pal(palette = 'RdYlBu')(9),
    'RdGy' = scales::brewer_pal(palette = 'RdGy')(9),
    'RdBu' = scales::brewer_pal(palette = 'RdBu')(9),
    'PuOr' = scales::brewer_pal(palette = 'PuOr')(9),
    'PRGn' = scales::brewer_pal(palette = 'PRGn')(9),
    'PiYG' = scales::brewer_pal(palette = 'PiYG')(9),
    'viridis' = substr(viridis::viridis(9),1,7),
    'magma' = substr(viridis::magma(9),1,7),
    'inferno' = substr(viridis::inferno(9),1,7),
    'plasma' = substr(viridis::plasma(9),1,7),
    'cividis' = substr(viridis::cividis(9),1,7)
  )
  palette_rev <- lapply(palette, function(x){rev(x)})
  names(palette_rev) <- paste0(names(palette_rev), "(rev)")
  c(palette, palette_rev)[unlist(lapply(1:31, function(x){c(x, x + length(palette))}))]
}


set_girafe <- function(x){
  x <- girafe_options(x, opts_selection(type = "single", css = "fill:red;stroke:gray;r:5pt;") )
  girafe_options(x,
                 opts_hover(css = "fill:red;stroke:white;r:5pt;") )
}




