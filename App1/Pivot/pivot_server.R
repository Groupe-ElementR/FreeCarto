output$pivotApp <- renderRpivotTable({
  rpivotTable(data = baseData$data)
})