source("data.R")

logo = grid::rasterGrob(png::readPNG("www/watermark.png"))

lup = format(Sys.time(), '%d %B, %Y')

## app ####

ui <- dashboardPage(
    dashboardHeader(title = "Piecing together the COVID-19 puzzle in India - Elika Garg", titleWidth = "50%",
    dropdownMenu(type="notification",
                 notificationItem(
                     text = paste0("Updated : ",lup), icon=icon("calendar")
                 )
                 )
                 ),
    dashboardSidebar(sidebarMenu(
        menuItem("About", tabName = "about",
                 badgeLabel = "info", badgeColor = "blue"),
        menuItem("Demographics", tabName = "b1",
                 badgeLabel = "1", badgeColor = "green"),
        menuItem("Testing", tabName = "b2",
                 badgeLabel = "2", badgeColor = "green"),
        menuItem("State Summaries", tabName = "b3",
                 badgeLabel = "3", badgeColor = "green"),
        menuItem("State Testing", tabName = "b401",
                 badgeLabel = "4.1", badgeColor = "green"),
        menuItem("State Dailies", tabName = "b402",
                 badgeLabel = "4.2", badgeColor = "green"),
        menuItem("District Summaries", tabName = "b501",
                 badgeLabel = "5.1", badgeColor = "green"),
        menuItem("Top Districts", tabName = "b502",
                 badgeLabel = "5.2", badgeColor = "green"),
        menuItem("Lockdown Phases", tabName = "b6",
                 badgeLabel = "6", badgeColor = "green")
    )),
    dashboardBody(tags$head(includeHTML("google-analytics.html")),
        tabItems(tabItem(
            tabName = "about",
            h3("Status"), em("At last update of this data on ", lup, " the total number of active patients in India is ",strong(format(nstatus[["active"]],big.mark=","))," in addition to ",strong(format(nstatus[["recovered"]],big.mark=","))," encouraging recoveries and ",strong(format(nstatus[["deaths"]],big.mark=","))," regrettable deaths, bringing the total number of confirmed cases to ",strong(format(nstatus[["confirmed"]],big.mark=",")),"."), br(), em(strong(format(min(nunassign),big.mark=",")), " active patients from above total have not been assigned to any state."), hr(),
         h3("Data sources"),
            a("Publicly-available crowd-sourced COVID-19-India data", href = "https://www.covid19india.org/", target = "_blank"), br(),
            a("World testing numbers", href = "https://ourworldindata.org/covid-testing", target = "_blank"), br(),

            a("Population of Indian states projected for 2020", href = "https://uidai.gov.in/images/state-wise-aadhaar-saturation.pdf", target = "_blank")
 ),
        tabItem(tabName = "b1",
                h2("Patient Demographics"), plotOutput("p1", height = 1000), h6("Top left panel shows the number of patients in each age group. Top right panel shows the number of males and females. Middle panel shows the number of males and females separately in each age group. Bottom panel shows the percentage of each gender in each age group. The dashed line is indicative of equal infection share. The numbers on top indicate the total number of infections in that age group."), hr()
        ),

        tabItem(tabName = "b2",
                h2("Testing Rates"), plotOutput("p2", height = 1000), h6("Top panel compares India against other countries with respect to latest testing numbers. Comparison for total tests and their numbers adjusted by population size is vital to see the full picture. Bottom panel shows India's testing numbers increasing with time. Gaps in data occur when official counts are not reported. Current testing method can incorrectly report a positive infection as a negative case 30% of the time. This necessitates multiple tests per person, which leads to the reported number of samples tested as always greater than the number of persons tested."), hr()
        ),

        tabItem(tabName = "b3",
                h2("State-wise Summaries"), plotOutput("p3", height = 1000), h6("Cumulative state-wise counts. Number of active cases is seen against number of recoveries in the top panel (full data) and against outcome ratio (recoveries/deaths) in the bottom panel (zoomed in for states with more than 100 active cases)."), hr()
                ),

        tabItem(tabName = "b401",
                h2("State-wise Testing"), plotOutput("p401", height = 1500), h6("State-wise testing. Cumulative state testing is pitted against state population in the top panel and stretched out by day in the bottom panel."), hr()
                ),

        tabItem(tabName = "b402",
                h2("State-wise Time-series"), plotOutput("p402", height = 1500), h6("Daily state-wise counts. Daily count of confirmed cases and deaths per state is in the top panel and proportion of daily counts is in the bottom panel."), hr()
                ),

        tabItem(tabName = "b501",
                h2("District-wise Summaries"), plotOutput("p501", height = 1000), h6("Cumulative district-wise counts. Number of active cases is seen against number of recoveries. Districts with more than 1000 confirmed cases are labelled."), hr()
        ),

        tabItem(tabName = "b502",
                h2("Most affected districts in a state"), plotOutput("p502", height = 1500), h6("Districts that either have more than 1000 confirmed cases, or their proportion in their state is higher than 10% for confirmed cases or deaths are shown. All of these districts have more than 100 active cases."), hr()
        ),

        tabItem(tabName = "b6",
                h2("Most affected states during lockdown phases"), plotOutput("p6", height = 1500), h6("COVID-19 cumulative numbers during various restriction phases in most-affected states with complete data. Proportion of various parameters is seen for each phase."), hr()
        )
    ))
)

server <- function(input, output, session) {

    # p1 ####

    p.a = ipd %>%
        dplyr::filter(!is.na(Age)) %>%
        ggplot(aes(x=Age)) +
        geom_histogram(fill="ivory",alpha=0.75,color="black",breaks=seq(0,100,10)) +
        stat_bin(breaks=seq(0,100,10), geom="label", colour="black",aes(label=..count..), position=position_stack(vjust=1), vjust=-0.1) +
        scale_x_continuous(breaks=seq(0,100,10)) +
        scale_y_continuous(expand = expansion(c(0,0.3))) +
        ggthemes::theme_fivethirtyeight() +
        labs(title = "Patient Age", subtitle = paste0("Number of cases = ", nrow(dplyr::filter(ipd,!is.na(Age)))),caption = "Each bar represents a window of 10 years", x="Age", y="Count") +
        theme(axis.title = element_text())

    p.g = ipd %>%
        dplyr::filter(!is.na(Gender)) %>%
        dplyr::count(Gender) %>%
        ggplot(aes(x=Gender, y=n, label = n)) +
        geom_col(aes(fill=Gender),color="black",alpha=0.75,,show.legend=F,width=0.5) +
        geom_label(vjust=1) +
        ggthemes::theme_fivethirtyeight() +
        labs(title = "Patient Gender", subtitle = paste0("Number of cases = ", nrow(dplyr::filter(ipd,!is.na(Gender)))),caption = "", x="Gender", y="Count") +
        theme(axis.title = element_text()) +
        scale_fill_ordinal(option="E")

    p.ag1 = ipd %>%
        dplyr::filter_at(vars(Age,Gender),all_vars(!is.na(.))) %>%
        ggplot(aes(x=Age,fill=Gender)) +
        geom_histogram(alpha=0.75,color="black",breaks=seq(0,100,10), position="dodge",show.legend = F) +
        scale_x_continuous(breaks=seq(0,100,10)) +
        ggthemes::theme_fivethirtyeight() +
        labs(title = "Patient Age-Gender", subtitle = paste0("Number of cases = ", nrow(dplyr::filter_at(ipd,vars(Age,Gender),all_vars(!is.na(.))))), x="Age", y="Count") +
        theme(axis.title.y = element_text()) +
        scale_fill_ordinal(option="E")


    p.ag2 = ipd %>%
        dplyr::filter_at(vars(Age,Gender),all_vars(!is.na(.))) %>%
        ggplot(aes(x=Age,fill=Gender)) +
        geom_histogram(alpha=0.75,color="black",breaks=seq(0,100,10), position="fill") +
        stat_bin(breaks=seq(0,100,10), geom="label", colour="black",fill="white",aes(label=..count..),position=position_fill(vjust=1), vjust=-0.1) +
        scale_x_continuous(breaks=seq(0,100,10)) +
        geom_hline(yintercept = 0.5,linetype=2,color=scales::muted("red")) +
        ggthemes::theme_fivethirtyeight() +
        labs(caption = "Each bar represents a window of 10 years", x="Age", y="Percentage") +
        theme(axis.title = element_text()) +
        scale_y_continuous(labels = scales::percent_format(),expand = c(0,0.2), breaks = c(0.25,0.5,0.75)) +
        scale_fill_ordinal(option="E")

    p.ag = cowplot::plot_grid(plotlist=list(p.a,p.g),nrow=1,rel_widths = c(1,0.7))
    p1 = cowplot::plot_grid(plotlist=list(p.ag,p.ag1,p.ag2),ncol=1,rel_heights = c(1,0.7,1))

    output$p1 = renderPlot(cowplot::plot_grid(plotlist=list(p1,logo),ncol=1,rel_heights = c(10,1)))

    # p2 ####

    # world

    owd.out = owd.tests %>% mutate(Ranks.total = rank(Tests.total), Ranks.perp = rank(Tests.perp), Ranks.prod = Ranks.total*Ranks.perp) %>% dplyr::filter_at(vars(contains("ranks")), any_vars(.<sort(.)[10] | .>sort(.,decreasing = T)[10])) %>% mutate_at(vars(contains("rank")), rank) %>% mutate(outliers = case_when(Ranks.prod<median(Ranks.prod) & Ranks.total<median(Ranks.total) & Ranks.perp<median(Ranks.perp)~"less",Ranks.prod>median(Ranks.prod) & Ranks.total>median(Ranks.total) & Ranks.perp>median(Ranks.perp)~"more")) %>% dplyr::select(Country,outliers)

    owd.tests %<>% full_join(owd.out)

    owd.ind = dplyr::filter(owd.tests,Country=="I N D I A")

    p.tw = owd.tests %>%
        ggplot(aes(x=Tests.total,y=Tests.perp,label=Country,fill=outliers)) +
        ggrepel::geom_label_repel(aes(color = is.na(outliers)&Country!="I N D I A"),min.segment.length = 0) +
        geom_point() +
        scale_y_log10(labels = scales::label_number_si()) +
        scale_x_log10(labels = scales::label_number_si()) +
        ggthemes::theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.position="none") +
        labs(title = "World Tests", y = "Tests done per thousand people", x = "Total tests done", caption = "Axes are on logarithmic scales\nColors indicate the highest (green) and the lowest (red) testing countries by either measure") +
        scale_fill_manual(values=c(less="rosybrown1", more="darkseagreen1")) +
        annotate(geom="point",color="mediumblue", x = owd.ind$Tests.total, y = owd.ind$Tests.perp,size=3,pch=21,fill="red") +
        scale_color_manual(values=c(`TRUE`="grey",`FALSE`="black"))


    # positive


    percentsamples = tested %>% top_n(1,totalconfirmed) %>% mutate(ratio=(totalconfirmed)/(totalsamplestested)) %>% pull(ratio) %>% scales::percent(.,accuracy = 0.01)

    percentpersons = tested %>% top_n(1,totalconfirmed) %>% mutate(ratio=(totalconfirmed)/(totalindividualstested)) %>% pull(ratio) %>% scales::percent(.,accuracy = 0.01)

    conpos = dplyr::filter(tested, totalpositivecases>0) %$%
        cor(totalpositivecases,totalconfirmed) %>% scales::percent(.,accuracy = 0.01)

    p.tp = tested %>% # replace totalpositivecases with totalconfirmed in tested
        gather("Tested","value",c("totalsamplestested","totalindividualstested")) %>%
        mutate(Tested=ifelse(Tested=="totalsamplestested","Samples","Persons"), ratio = totalconfirmed/value) %>%
        # gather("keyer","ratio",c("ratio1","ratio2")) %>%
        ggplot(aes(x=date,y=ratio,group=Tested,color=Tested)) +
        geom_step(size=3,alpha=0.7) +
        geom_point(fill="white",color="black",pch=21,size=3) +
        scale_y_continuous(labels = scales::percent_format()) +
        ggthemes::theme_fivethirtyeight() +
        labs(title = "Confirmed Cases", subtitle = paste0("Confirmed Samples = ", percentsamples, "  |  Confirmed Persons = ", percentpersons),caption = "Each dot represents available data point\nClosely tracking bold lines show the different testing numbers", x="Date", y="Percentage") +
        theme(axis.title = element_text()) +
        scale_color_manual(values=c("Persons"="grey70","Samples"="grey40"))

    p2 = cowplot::plot_grid(plotlist=list(p.tw,p.tp),ncol=1,rel_heights = c(3,2))

    output$p2 = renderPlot(cowplot::plot_grid(plotlist=list(p2,logo),ncol=1,rel_heights = c(15,1)))

    # p3 ####

    p.recac = stst[!grepl("Total|State",state)] %>% mutate(type = as_factor(case_when(confirmed==0~"None",deaths==0~"Best",recovered/active>0.5 | active<10~"Good",recovered/active<0.5~"Bad"))) %>%  ggplot(aes(y=recovered,x=active,label=state,color=type)) +
        geom_smooth(data = stst[recovered>0 & active>0 & !grepl("Total|State",state)], method="lm",se = F,linetype = 2, color="lightgrey") +
        geom_point(aes(size=ifelse(deaths==0,5,deaths),shape=deaths==0)) +
        ggrepel::geom_text_repel(point.padding = 0.5,min.segment.length = 0,show.legend = FALSE) +
        scale_shape_manual(values = c(`TRUE`= 5, `FALSE` = 19)) +
        scale_color_manual(values = c(Best="steelblue",Bad="peru",Good = "olivedrab4", None="lightseagreen"), labels = c(Good = "High recovery", Bad = "Low recovery", Best = "No deaths", None = "No cases")) +
        scale_y_continuous(trans = scales::pseudo_log_trans(sigma=0.1),
                           breaks = c(0,1,10,100,1000,5000,30000)) +
        scale_x_continuous(trans = scales::pseudo_log_trans(sigma=1),
                           breaks = c(0,1,10,100,1000,5000,30000)) +
        ggthemes::theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.position = c(0.8,0.2), legend.title = element_blank(), legend.direction = "vertical", legend.background = element_rect(fill="white")) +
        labs(title = "State-wise: Recovery", y = "Number of recovered cases", x = "Number of active cases") +
        guides(shape = FALSE, size = FALSE)

    p.surcon = stst[survival>0 & confirmed>0 & active>100 & !grepl("Total|State",state)] %>% mutate(type = as_factor(case_when(confirmed==0~"None",deaths==0~"Best",recovered/active>0.5 | active<10~"Good",recovered/active<0.5~"Bad"))) %>% ggplot(aes(y=survival,x=active,label=state,color=type)) +
        geom_smooth(method="lm",se = F,linetype = 2, color="lightgrey") +
        ggrepel::geom_text_repel(point.padding = 0.5,min.segment.length = 0) +
        geom_point(aes(size=deaths)) +
        scale_color_manual(values = c(Best="steelblue",Bad="peru",Good = "olivedrab4", None="lightseagreen"), labels = c(Good = "High recovery", Bad = "Low recovery", Best = "No deaths", None = "No cases")) +
        scale_y_continuous(trans = scales::pseudo_log_trans(sigma=0.01),
                           breaks = c(0,1,10,100,1000,5000,30000)) +
        scale_x_continuous(trans = scales::pseudo_log_trans(sigma=1),
                           breaks = c(0,1,10,100,1000,5000,30000)) +
        ggthemes::theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.position="none") +
        labs(title = "State-wise: Outcome ratio", y = "Ratio of recoveries and deaths", x = "Number of active cases", caption = "Axes are on logarithmic scales\nSizes represent deaths | Colors represent outcome categories")

    p3 = cowplot::plot_grid(plotlist=list(p.recac,p.surcon),ncol=1,rel_heights = c(3,2))
    output$p3 = renderPlot(cowplot::plot_grid(plotlist=list(p3,logo),ncol=1,rel_heights = c(15,1)))

    # p401 ####

    p.tst = full_join(stst[!grepl("Total|State",state)],tst) %>% inner_join(pop) %>% mutate(type = as_factor(case_when(confirmed==0~"None",deaths==0~"Best",recovered/active>0.5 | active<10~"Good",recovered/active<0.5~"Bad")), tests=replace_na(tests,0), posper = confirmed/tests, survival = ifelse(survival==0|is.na(survival),0,survival), posper = ifelse(posper==0|is.na(posper),0,posper)) %>% ggplot(aes(x=population,y=tests,label=state,color=type)) +
        geom_smooth(method="lm",se = F,linetype = 2, color="lightgrey") +
        geom_point(aes(size=ifelse(deaths==0,5,deaths),shape=deaths==0)) +
        ggrepel::geom_text_repel(point.padding = 0.5,min.segment.length = 0,show.legend = FALSE) +
        scale_shape_manual(values = c(`TRUE`= 5, `FALSE` = 19)) +
        scale_color_manual(values = c(Best="steelblue",Bad="peru",Good = "olivedrab4", None="lightseagreen"), labels = c(Good = "High recovery", Bad = "Low recovery", Best = "No deaths", None = "No cases")) +
        scale_y_continuous(trans = scales::pseudo_log_trans(sigma=5e3),
                           breaks = c(0,1e4,1e5,1e6), labels = scales::label_number_si()) +
        scale_x_continuous(trans = scales::pseudo_log_trans(sigma=1e6),
                           breaks = c(1e4,1e6,1e7,1e8,2e8), labels = scales::label_number_si()) +
        ggthemes::theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.position = c(0.1,0.8), legend.title = element_blank(), legend.direction = "vertical", legend.background = element_rect(fill="white")) +
        labs(title = "State-wise: Cumulative testing", y = "Number of tests", x = "State population", caption = "Axes are on logarithmic scales\nSizes represent deaths | Colors represent outcome categories") +
        guides(shape = FALSE, size = FALSE)

    p.tstd = tstd %>%
        dplyr::filter(totaltested>0) %>%
        mutate(state=fct_reorder(state,population)) %>%
        ggplot(aes(x=date,y=state,fill=totaltested*1e3/population)) +
        geom_tile() +
        scale_fill_viridis_c(option = "B",name="Tests Per 1000",direction=-1,alpha=0.7,end=0.75) +
        scale_y_discrete(position="right") +
        facet_grid(sized~.,scales="free") +
        ggthemes::theme_fivethirtyeight() +
        theme(legend.key.width = unit(30,"pt"), legend.title = element_text(vjust=0.8), strip.placement = "outside", panel.spacing.y = unit(1,"pt"), plot.caption.position="plot") +
        labs(title = "State-wise: Daily testing", caption="States are arranged and categorized by relative population")

    p401 = cowplot::plot_grid(plotlist=list(p.tst,p.tstd),ncol=1)
    output$p401 = renderPlot(cowplot::plot_grid(plotlist=list(p401,logo),ncol=1,rel_heights = c(15,1)))

    # p402 ####
    p.dst = ststd %>% inner_join(pop) %>% dplyr::rename_all(str_to_title) %>%
        mutate(Month_announced =month(Date,label=TRUE) %>% fct_collapse("Jan-Feb" = c("Jan","Feb")) %>% fct_drop) %>%
        mutate(State = fct_reorder(State,Population)) %>%
        ggplot(aes(y=State,x=Date,color=log10(Deaths))) +
        geom_point(aes(size=ifelse(Confirmed==0&Deaths>0,0.1,log10(Confirmed))),alpha=0.9) +
        facet_grid(Sized~Month_announced,scales = "free",space = "free") +
        labs(title = "State-wise: Daily accumulation of cases", subtitle=str_c(format(min(ststd$date),"%b %d"), format(max(ststd$date),"%b %d"), sep=" - "),
             caption = "States are arranged and categorzied by relative population\nSizes represent daily confirmed cases | Colors represent daily deaths\nDarker notes mean more deaths | Grey means zero deaths") +
        ggthemes::theme_fivethirtyeight() +
        theme(legend.position="none",panel.grid.major.x = element_blank(),axis.text.x = element_blank(), strip.placement = "outside", panel.spacing.y = unit(0.01,"pt"), plot.caption.position="plot") +
        scale_color_viridis_c(alpha = 0.7,option = "C",end=0.75, direction=-1) +
        scale_y_discrete(position="right", expand = c(0,2)) +
        scale_size_binned_area()

    p.dast = ststd %>%
        inner_join(pop) %>%
        mutate(sized = fct_rev(factor(cut_number(population, 3),labels = c("Small","Medium","Large")))) %>%
        gather("key","value",c(confirmed,deaths,recovered)) %>%
        dplyr::select(key,value,sized,date,state) %>%
        distinct() %>%
        group_by(sized,key) %>%
        arrange(date) %>%
        mutate(value = frollmean(value, 14,align="center")) %>%
        group_by(date,sized) %>%
        mutate(total=sum(value,na.rm=T)) %>%
        group_by(total,key, .add=T) %>%
        summarise(value=sum(value,na.rm=T)) %>%
        ungroup %>%
        mutate(prop = value/total, key=fct_relevel(str_to_title(key),"Deaths")) %>%
        ggplot(aes(y=prop,x=date,fill=key)) +
        geom_area(alpha=0.9) +
        facet_grid(sized~.) +
        scale_fill_ordinal(name = "Daily status") +
        scale_y_continuous(labels = scales::percent_format(), breaks=c(0.25,0.50,0.75)) +
        labs(y = "Proportion of daily totals", title = "State-wise: Daily proportion of cases", caption = "States are categorzied by relative population") +
        ggthemes::theme_fivethirtyeight() +
        theme(axis.title.x = element_blank())

    p402 = cowplot::plot_grid(plotlist=list(p.dst,p.dast),ncol=1,rel_heights = c(3,2))
    output$p402 = renderPlot(cowplot::plot_grid(plotlist=list(p402,logo),ncol=1,rel_heights = c(20,1)))

    # p501 ####

    p501 = dstt %>% inner_join(statewise[,c("state","statecode")]) %>%
        mutate(oul=ifelse(confirmed>1000, paste0(district," (",statecode,")"), NA), survival = replace_na(survival,100)) %>%
        ggplot(aes(x=active,y=recovered, size=ifelse(deaths>0,log10(deaths),0.1), shape=deaths==0, color=survival<fivenum(dstt[,survival])[2])) +
        geom_smooth(data = dstt[confirmed>500], method="lm",se = F,linetype = 2, color="lightgrey") +
        geom_point(aes(alpha=log10(confirmed))) +
        ggrepel::geom_text_repel(size=3, aes(label=oul),point.padding = 0.5,min.segment.length = 0, show.legend = F, ) +
        scale_y_continuous(trans = scales::pseudo_log_trans(sigma=50),
                           breaks = c(0,100,1000,5000,10000)) +
        scale_x_continuous(trans = scales::pseudo_log_trans(sigma=50),
                           breaks = c(0,100,1000,5000,10000)) +
        scale_color_manual(values=c("darkslategrey","violetred"), labels=c(`TRUE` = "Low", `FALSE`= "High"), name="Recoveries : Deaths") +
        scale_shape_manual(values = c(`TRUE`= 5, `FALSE` = 19)) +
        ggthemes::theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.position = c(0.8,0.2), legend.direction = "horizontal", legend.background = element_rect(fill="white"), legend.title.align = 0.5, legend.key.width = unit(40,"pt")) +
        labs(title = "District-wise: Recovery", y = "Number of recovered cases", x = "Number of active cases", caption = "Axes are on logarithmic scales\nOpacity represents confirmed cases\nSizes represent deaths | Squares mean no deaths") +
        guides(shape = FALSE, size = FALSE, alpha = FALSE, colour = guide_legend(title.position = "top"))

    output$p501 = renderPlot(cowplot::plot_grid(plotlist=list(p501,logo),ncol=1,rel_heights = c(10,1)))

    # p502 ####
    p502 = dstts %>% group_by(state) %>% summarise_at(vars(confirmed,active,recovered,deaths),sum) %>% dplyr::rename_at(vars(-state), paste0, ".SUM") %>% inner_join(dstt [,N:=.N,by="state"][active>100&N>1]) %>% mutate(confirmed.PERC = confirmed/confirmed.SUM, deaths.PERC = deaths/deaths.SUM) %>% dplyr::filter(confirmed.PERC>0.1 | deaths.PERC>0.1 | confirmed>1000) %>% dplyr::rename_at(vars(confirmed,deaths),paste0,".VALUE") %>% dplyr::select(state,district,contains(".PERC"),contains(".VALUE")) %>% gather("key","value",-c(1,2)) %>% separate("key",c("key","sprea")) %>% spread("sprea","value") %>% dplyr::rename_all(str_to_lower) %>% mutate(perc=replace_na(perc,0), key=str_to_title(key)) %>% inner_join(statewise[,c("state","statecode","confirmed")]) %>% mutate(district=fct_reorder(district,perc,max), statecode=fct_reorder(statecode,confirmed,max)) %>%
        ggplot(aes(y=district,x=key,fill=log10(perc))) +
        geom_tile(size=1) +
        geom_label(aes(label=str_c(scales::label_number_si()(value), " (",scales::percent(round(perc,2),accuracy=1),")")),fill="white",label.size = 0,hjust=1,x=Inf) +
        scale_fill_distiller(direction = 1, palette = "YlOrRd") +
        facet_grid(statecode~key,scales="free",space="free",switch = "y",as.table = F) +
        ggthemes::theme_fivethirtyeight() +
        theme(legend.position = "none") +
        labs(title = "District-wise: Most affected in State", caption = "Text indicates district total number and percentage in its state\nColor represents percentage as well")

    output$p502 = renderPlot(cowplot::plot_grid(plotlist=list(p502,logo),ncol=1,rel_heights = c(20,1)))

    # p6 ####

    p6 = inner_join(tstd[state %in% dplyr::filter(statewise,confirmed>10000&state!="Total")$state][date>="2020-03-25"][,.(population,date,state,tested=totaltested)],ststd) %>% group_by(state) %>%
        mutate_at(vars(confirmed,deaths,recovered),cumsum) %>%
        dplyr::select(population,state,date,confirmed,deaths,recovered,tested) %>%
        mutate(active = confirmed-recovered-deaths) %>%
        gather("key","value",-c("population","state","date")) %>%
        mutate(value = ifelse(value<0,NA,value)) %>%
        mutate(Phase=case_when(date>="2020-06-01"~5,date>="2020-05-18"~4,date>="2020-05-04"~3,date>="2020-04-15"~2,date>="2020-03-25"~1)) %>%
        group_by(Phase,state,key,population) %>%
        summarise(value=max(value,na.rm = T)) %>%
        ungroup %>%
        spread("Phase","value") %>%
        mutate(total = `5`,`2` = `2`-`1`, `3` = `3`-`2`, `4` = `4`-`3`, `5` = `5`-`4`) %>%
        remove_missing() %>%
        gather("Phase","value",-c("state","key","total","population")) %>%
        mutate(Phase = as.integer(Phase)) %>%
        remove_missing() %>%
        mutate(state = fct_reorder(str_c(str_wrap(state,5),scales::label_number_si()(population),sep="\n\n"),population), key = fct_relevel(str_to_sentence(key),"Tested","Confirmed","Recovered","Deaths","Active")) %>%
        ggplot(aes(fill=fct_rev(as.factor(Phase)), y=value, x=state)) +
        geom_col(position="fill",color="black",alpha=0.7) +
        geom_text(aes(label=scales::label_number_si()(total)), y=1.1, check_overlap = T) +
        facet_grid(key~.,scales="free", switch = "y") +
        ggthemes::theme_fivethirtyeight() +
        scale_fill_brewer(type="seq",name="Phase") +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(c(0,0.3)), breaks=c(0.25,0.50,0.75), position = "right") +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks = element_line()) +
        labs(title = "Phase-wise: COVID-19 proportions", caption = "Numbers on top of each glass indicate the rounded-off value at 100%\nNumbers below state names indicate their rounded-off projected population\nStates are arranged in magnitude of their population")

    output$p6 = renderPlot(cowplot::plot_grid(plotlist=list(p6,logo),ncol=1,rel_heights = c(20,1)))

}

shinyApp(ui = ui, server = server)
