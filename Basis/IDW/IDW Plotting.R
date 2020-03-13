# IDW Plotting

soybeanIDW = readRDS("Basis/IDW/soybeanIDW.rds")
soybeanOctIDW = readRDS("Basis/IDW/soybeanOctIDW.rds")
soybeanJanIDW = readRDS("Basis/IDW/soybeanJanIDW.rds")
cornIDW = readRDS("Basis/IDW/cornIDW.rds")
cornOctIDW = readRDS("Basis/IDW/cornOctIDW.rds")
cornJanIDW = readRDS("Basis/IDW/cornJanIDW.rds")


# tmap_mode("view")
# tmap_mode("plot")
# tmaptools::palette_explorer() 

if (type == "corn") {
  currentIdwr = cornIDW[["idwr"]]
  midPoint = cornIDW[["midPoint"]]
  basisValues = cornIDW[["basisSP"]]
  
  octIdwr = cornOctIDW[["idwr"]]
  octMidPoint = cornOctIDW[["midPoint"]]
  octBasisValues = cornOctIDW[["basisSP"]]
  
  janIdwr = cornJanIDW[["idwr"]]
  janMidPoint = cornJanIDW[["midPoint"]]
  janBasisValues = cornJanIDW[["basisSP"]]
  
  title = tm_layout(title = "Missouri Corn: 2/28/2020")
}

if (type == "soybean") {
  currentIdwr = soybeanIDW[["idwr"]]
  currentMidPoint = soybeanIDW[["midPoint"]]
  currentBasisValues = soybeanIDW[["basisSP"]]
  
  octIdwr = soybeanOctIDW[["idwr"]]
  octMidPoint = soybeanOctIDW[["midPoint"]]
  octBasisValues = soybeanOctIDW[["basisSP"]]
  
  janIdwr = soybeanJanIDW[["idwr"]]
  janMidPoint = soybeanJanIDW[["midPoint"]]
  janBasisValues = soybeanJanIDW[["basisSP"]]
  
  title = tm_layout(title = "Missouri Soybeans: 2/28/2020")
}

# Plot two maps in one
tm = tm_shape(currentIdwr) + 
  tm_raster(n = 15, palette = "RdYlBu", contrast = c(0.4, 1), midpoint = currentMidPoint,
            title = "Basis (cents)", legend.reverse = TRUE, group = "Current") + 
  tm_shape(currentBasisValues) + 
  tm_dots(size = 0.1, col = "black", popup.vars = c("City" = "City", "Terminal" = "Terminal"), group = "Current") +
  tm_legend(legend.outside = TRUE) +
  title + 
  
  tm_shape(octIdwr) + 
  tm_raster(n = 15, palette = "RdYlBu", contrast = c(0.4, 1), midpoint = octMidPoint,
            title = "Basis (cents)", legend.reverse = TRUE, group = "October") + 
  tm_shape(octBasisValues) + 
  tm_dots(size = 0.1, col = "black", popup.vars = c("City" = "City", "Terminal" = "Terminal"), group = "October") +
  tm_legend(legend.outside = TRUE) +
  title + 
  
  tm_shape(janIdwr) + 
  tm_raster(n = 15, palette = "RdYlBu", contrast = c(0.4, 1), midpoint = janMidPoint,
            title = "Basis (cents)", legend.reverse = TRUE, group = "January") + 
  tm_shape(janBasisValues) + 
  tm_dots(size = 0.1, col = "black", popup.vars = c("City" = "City", "Terminal" = "Terminal"), group = "January") +
  tm_legend(legend.outside = TRUE) +
  title 

tm %>%
  tmap_leaflet() %>%
  addLayersControl(baseGroups = c("Current", "October", "January"), 
                   position = "topleft",
                   options = layersControlOptions(collapsed = F)) %>%
  onRender("
    function(el, x) {
      var legends = document.querySelectorAll('.legend');
      
      function hideAllLegends(item, index) {
        item.hidden = true;
      }
    
      legendsArray = {
                'Title': legends[0],
                'Current': legends[1],
                'October': legends[2],
                'January': legends[3]
      };
    
      legends[1]
    
      var updateLegend = function () {
      
        legends.forEach(hideAllLegends);
        
        if(document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1) === 'Current') {
          legendsArray['Title'].hidden = false;
          legendsArray['Current'].hidden = false;
        }
        if(document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1) === 'October') {
          legendsArray['Title'].hidden = false;
          legendsArray['October'].hidden = false;
        }
        if (document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1) === 'January') {
          legendsArray['Title'].hidden = false;
          legendsArray['January'].hidden = false;
        }
          
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }")


