#library(htmlwidgets)

socal_murder_map <- leaflet(murders_region, options = leafletOptions(zoomControl = FALSE, zoomSnap = 0.5, zoomDelta=0.5)) %>%
  htmlwidgets::onRender("function(el, x) {
L.control.zoom({ position: 'topright' }).addTo(this)
}") %>%
  setView(-118.24, 34.05, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", 
              popup = murderlabel_region,
              popupOptions = popupOptions(maxWidth ="200", 
                                          minWidth ="200"),
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 0.6, 
              fillOpacity = 0.6,
              fillColor = ~murderpal1(rate21),
              group="Rate") %>% 
  addPolygons(color = "white", 
              popup = murderlabel_region,
              popupOptions = popupOptions(maxWidth ="200", 
                                          minWidth ="200"),
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 0.6, 
              fillOpacity = 0.6,
              fillColor = ~murderpal2(total21),
              group="Number") %>% 
  addPolygons(data = murders_district,
              color = "yellow", 
              popup = murderlabel,
              popupOptions = popupOptions(maxWidth ="200", 
                                          minWidth ="200"),
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 0.25, 
              fillOpacity = 0.3,
              fillColor = "purple",
              group="Rate") %>% 
  addSearchOSM(options = searchOptions(autoCollapse=FALSE, minLength = 3,zoom=13, position="topleft")) %>%
  onRender("function(el, x) {
        $('input.search-input')[0].placeholder = 'Search street, place or zip code'
        }") %>%
  addLegend(opacity = 0.6,
            values = murders_region$rate21, 
            pal = murderpal1,
            position = "bottomleft", 
            title = paste(sep="","<popuptitle>Murder Rate<br><popupchatter>Murders/100K people<br>"),
            group = "Rate",
            className = "info legend Rate") %>%
  addLegend(opacity = 0.6,
            values = murders_region$total21, 
            pal = murderpal2,
            position = "bottomleft", 
            title = paste(sep="","<popuptitle>Murders<br>"),
            group = "Number",
            className = "info legend Number") %>%
  addLayersControl(
    baseGroups = c("Rate","Number"),
    options = layersControlOptions(collapsed = FALSE),
    position = 'bottomright') %>% hideGroup(c("Number")) %>%
  htmlwidgets::onRender("
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedGroup)) l.hidden=false;
            });
         };
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }"
  )

socal_murder_map