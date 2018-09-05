#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#p_marker='CD45+'
# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
   
    infile = reactive({
        req(input$input_file)
        inFile <- input$input_file
        if (is.null(inFile)) return(NULL)
        print(inFile$datapath)
        inFile$datapath
    })
    
    observe({
        toggleState('submit',input$input_file)
    })
    
    observeEvent(input$reset, {
        shinyjs::reset("input")
        shinyjs::disable('submit')
    })  
    
    observeEvent(input$submit,{
        updateNavbarPage(session,"main",selected = 'Data')
    })
    
    #marker selection to calculate percentage
   
    
    raw_data = reactive({
        data=NULL
        if(grep('\\.csv$',infile())){
            data=read.csv(infile(),header=T,stringsAsFactors = F,check.names = F)
        }else if(grep('\\.xlsx?$',infile())){
            data=read_excel(infile(),1)
        }
        data
    })
    
    output$data_table = DT::renderDataTable({
        DT::datatable(data = raw_data(),caption = 'Raw Data Table',extensions = c('Buttons','FixedColumns'),
                      options = list( dom = 'Blfrtip',autoWidth=T,pageLength = 50,digits=3,
                                      buttons=c('copy','csv','excel','pdf','print'),fixedColumns=list(leftColumns=2)), 
                      rownames = FALSE) 
    })

   
    orig_vars = reactive(names(raw_data()))
    output$id_vars = renderUI(checkboxGroupInput(inputId = "ivs", 
                                            label = "Select ID variables:",
                                            choices = orig_vars(), 
                                            selected = orig_vars()[1],inline = T))
    observeEvent(input$ivs,{
        updateCheckboxGroupInput(session,'mvs',selected = orig_vars()[!orig_vars() %in% input$ivs])
    })
    output$measurement_vars = renderUI(checkboxGroupInput(inputId = "mvs", 
                                                     label = "Select measurement variables:",
                                                     choices = orig_vars(), 
                                                     selected = orig_vars(),inline = T))
    final_data = eventReactive(input$submit_cd,{
        data_long=raw_data()
        if(input$convert_data){
            data_long = melt(raw_data(),id.vars=input$ivs,measure.vars=input$mvs,
                             variable.name=input$v_name,value.name = input$m_name)
        }  
        colnames(data_long)=gsub('^(\\d+)$','d\\1',colnames(data_long))
        data_long
    })
    observeEvent(input$submit_cd,{
        updateNavlistPanel(session,"summary",selected = 'Variable definition')
    })
    ##select which variable is sample ID, which variable is group ID, which variable is measured variable
    #marker selection, markers in group
    vars = reactive(names(final_data()))
    output$sample_var = renderUI(selectInput(inputId = "sv", 
                                             label = "Select Sample ID variable:",
                                             choices = vars(), 
                                             selected = NULL))
    output$group_var = renderUI(selectInput(inputId = "gv", 
                                             label = "Select Group ID variable:",
                                             choices = vars(), 
                                             selected = NULL))
    output$split_var = renderUI(selectInput(inputId = "spv", 
                                            label = "Select variable to split dataset:",
                                            choices = vars(), 
                                            selected = NULL))
    output$analysis_var = renderUI(selectInput(inputId = "av", 
                                            label = "Select variable to analyze:",
                                            choices = vars(), 
                                            selected = NULL))
    
    data_summary = reactive({
        group_vars=c(input$gv)
        if(input$split_data){
            group_vars=c(input$spv,input$gv)
        }
        data_summary=summarySE(final_data(),measurevar = input$av,groupvars = group_vars,na.rm=T)
    })
    
    output$data_summary_table = DT::renderDataTable({
        DT::datatable(data = data_summary(),caption = 'Data Summary Table',extensions = c('Buttons','FixedColumns'),
                      options = list( dom = 'Blfrtip',autoWidth=T,pageLength = 50,digits=3,
                                      buttons=c('copy','csv','excel','pdf','print'),fixedColumns=list(leftColumns=2)), 
                      rownames = F) 
    }) 
    
    observeEvent(input$submit_vd,{
        updateNavlistPanel(session,"summary",selected = 'Summary statistics')
    })
    
    all_grp_cmp=reactive({
        p=ggboxplot(final_data(),x=input$gv,y=input$av,title=paste0('Overall comparison boxplot by Kruskal-Wallis test'),
                    add=c('jitter'),color=input$gv,xlab=input$gv)+scale_color_manual(values=grps_cols)+
            stat_compare_means(method='kruskal.test',aes(label = paste0("p=", ..p.format..)),label.x.npc = 'left',label.y.npc = 'top')+mytheme
        if(input$split_data) p=facet(p,facet.by = input$spv,scales='free',ncol=3)
        p
    })      
    output$all_grp_cmp = renderPlot(all_grp_cmp(),height=1000)

})
