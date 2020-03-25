# QS results dashboard - 040 - UI

wellPanel(
  actionButton("select", "Button"),
  conditionalPanel(condition = "input.tabs == 'tab_2'",
                   actionButton("select", "Button")),
  conditionalPanel(condition = "input.tabs == 'tab_2'",
                   actionButton("select", "Button"))
)
