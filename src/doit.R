library('sf');
library('dplyr');
library('ggplot2');
library('ggpubr');

if (FALSE) {
  # Toronto: DVP and Gardiner are CLASS=23, RANK=3. Sigh.
  majorRoadsShape <- st_read('../input/lrnf000r20a_e.shp',
     query=paste0("SELECT * FROM \"lrnf000r20a_e\" WHERE ",
     "(PRUID_L='35' OR PRUID_L='24') AND ",
     "(CSDNAME_R='Ottawa' OR CSDNAME_R='Gatineau') AND ",
     "(CLASS='10' OR CLASS='11' OR CLASS='12' OR ",
  #   "(CLASS='23' AND RANK <> '4' AND RANK <> '5'))"));
      "(RANK <> '4' AND RANK <> '5'))"));
  st_write(majorRoadsShape, '../input/majorRoads.shp');
  
  waterShape <- st_read('../input/lhy_000c16a_e.shp',
     query=paste0("SELECT * FROM \"lhy_000c16a_e\" WHERE ",
                  "(PRUID='35' OR PRUID='24') AND ",
                  "(NAME LIKE '%Ottawa River%' OR NAME LIKE '%Lac Desch%' OR NAME LIKE '%Rideau River%' OR NAME='Dows Lake' OR NAME='Lac Leamy' OR NAME LIKE '%Gatineau%')"));
  st_write(waterShape, '../input/ottawaRiver.shp');
}
majorRoadsShape <- st_read('../input/majorRoads.shp') %>%
  st_transform(st_crs(26923));
ottawaRiverShape <- st_read('../input/ottawaRiver.shp') %>%
  st_transform(st_crs(26923)) %>%
  st_simplify(dTolerance=100);

wardsShape <- st_read('../input/Wards.shp');
rlsShape <- st_read('../input/Territoires_RLS_2020.shp');
rlsShape$WARD_NUM <- as.integer(rlsShape$RLS_code);
rlsShape <- rlsShape %>% filter(WARD_NUM >= 700 & WARD_NUM < 800 & WARD_NUM != 712);
wardsShape$WARD_NUM <- as.integer(wardsShape$WARD_NUM);


# These are case *rates*, despite the names here.
cases <- read.csv('../input/covid_ottawa_geog.csv', fileEncoding='macintosh')[1:20];
colnames(cases)[4:ncol(cases)] <- sapply(colnames(cases)[4:ncol(cases)], function(x) { substring(x,2) });
# If the pop were here, this would convert to cases. But we don't do that.
temp <- (cases[,5:ncol(cases)] - cases[,4:(ncol(cases)-1)]);# * cases$pop / 100000;
temp[temp < 0] <- 0;
#colnames(temp) <- sapply(colnames(temp), function(x) { paste0("d_",x)});
colnames(temp) <- c('May 20-Jun 9', 'Jun 10-Jul 6', 'Jul 7-20', 'Jul 21-Aug 3', 'Aug 4-17', 'Aug 18-24', 'Aug 24-Sep 7', 'Sep 8-21', 'Sep 22-Oct 5', 'Oct 6-19', 'Oct 20-Nov 2', 'Nov 3-16', 'Nov 17-30', 'Dec 1-14', 'Dec 15-28', 'Dec 29-Jan 11');
temp[,1] <- temp[,1] / 3;
temp[,2] <- temp[,2] / (4-1/7);
temp[,3:5] <- temp[,3:5] / 2;
temp[,6] <- temp[,6] / 1;
temp[,7:16] <- temp[,7:16] / 2;
# This for cumulative
#wards2 <- cbind(cases[,1:2], cases[,4:ncol(cases)]);
# These two for "by period"
wards2 <- cbind(cases[,1:2], temp);
colnames(wards2)[3:ncol(wards2)]=colnames(cases)[5:ncol(cases)];
wards2$'16.Jun.20' <- wards2$'06.Jul.20';
wards2$'23.Jun.20' <- wards2$'06.Jul.20';
wards2$'30.Jun.20' <- wards2$'06.Jul.20';
wards2$'13.Jul.20' <- wards2$'20.Jul.20';
wards2$'27.Jul.20' <- wards2$'03.Aug.20';
wards2$'10.Aug.20' <- wards2$'17.Aug.20';
wards2$'31.Aug.20' <- wards2$'07.Sep.20';
wards2$'14.Sep.20' <- wards2$'21.Sep.20';
wards2$'28.Sep.20' <- wards2$'05.Oct.20';
wards2$'12.Oct.20' <- wards2$'19.Oct.20';
wards2$'26.Oct.20' <- wards2$'02.Nov.20';
wards2$'09.Nov.20' <- wards2$'16.Nov.20';
wards2$'23.Nov.20' <- wards2$'30.Nov.20';
wards2$'07.Dec.20' <- wards2$'14.Dec.20';
wards2$'21.Dec.20' <- wards2$'28.Dec.20';
wards2$'04.Jan.21' <- wards2$'11.Jan.21';
cases <- cbind(cases, temp);
# Highest value: 141.

wards <- rbind(wardsShape %>% st_transform(crs=26923) %>% select('WARD_NUM', 'geometry'),
               rlsShape %>% st_transform(crs=26923) %>% select('WARD_NUM', 'geometry'));

wards <- wards %>%
  left_join(cases %>% select(c(colnames(cases)[1:3],colnames(temp[7:ncol(temp)]))) %>%
              tidyr::pivot_longer(tidyr::contains('-'), names_to='date', values_to = 'caserate'),
            by = c('WARD_NUM' = 'wardnum'));
wards$date <- factor(wards$date, levels=colnames(temp[7:ncol(temp)]));
wardsLim <- as.list(st_bbox(wards %>% filter(WARD_NUM < 24)));
wardsLim$w <- wardsLim$xmax - wardsLim$xmin;
wardsLim$h <- wardsLim$ymax - wardsLim$ymin;
capitalLim <- st_bbox(wards %>% filter(WARD_NUM == '17'));
wardsLim <- st_bbox(c(
  capitalLim$xmin - wardsLim$w*0.3,
  capitalLim$ymin - wardsLim$h*0.2,
  capitalLim$xmax + wardsLim$w*0.3,
  capitalLim$ymax + wardsLim$h*0.2
))
ggplot(wards %>% filter(date %in% c('Oct 6-19', 'Nov 3-16', 'Dec 1-14', 'Dec 29-Jan 11'))) +
  facet_wrap(~date) +
  geom_sf(aes(geometry=geometry, fill=caserate), color='#00000020', size=0.2) +
  scale_fill_fermenter(palette='GnBu', direction=1, breaks=c(1:4*25,150,200,250,300), oob=scales::squish) +
  geom_sf(data=ottawaRiverShape, aes(geometry=geometry), color='#00000000', fill='#e0f0ff') +
  geom_sf(data=majorRoadsShape %>% filter(CLASS %in% c('11','12')), aes(geometry=geometry), size=0.2, color='#808080') +
  theme_minimal() +
  coord_sf(xlim=c(wardsLim$xmin, wardsLim$xmax), ylim=c(wardsLim$ymin, wardsLim$ymax),
           label_graticule='') +
  ggtitle('COVID-19 in Ottawa-Gatineau: weekly cases per 100,000 by ward/RLS') +
  labs(fill='Cases/100K',
       caption='Ottawa excl. LTC/RHs; Gatineau: incl. CSHLD/RI/RPAs');
ggsave('maps.png', width=6, height=4, dpi='print', device='png', units='in',scale=1.5);
#graphics::title(main='COVID19 cases per 100,00 in Ottawa by ward');

wards2 <- cbind(wards2)
wards2 <- wards2 %>% tidyr::pivot_longer(cols=3:ncol(wards2), names_to='date');
wards2$date <- gsub("(..)\\.(...)\\.(..)", "20\\3-\\2-\\1", wards2$date);
wards2$date <- gsub("Jan", "01", wards2$date);
wards2$date <- gsub("Feb", "02", wards2$date);
wards2$date <- gsub("Mar", "03", wards2$date);
wards2$date <- gsub("Apr", "04", wards2$date);
wards2$date <- gsub("May", "05", wards2$date);
wards2$date <- gsub("Jun", "06", wards2$date);
wards2$date <- gsub("Jul", "07", wards2$date);
wards2$date <- gsub("Aug", "08", wards2$date);
wards2$date <- gsub("Sep", "09", wards2$date);
wards2$date <- gsub("Oct", "10", wards2$date);
wards2$date <- gsub("Nov", "11", wards2$date);
wards2$date <- gsub("Dec", "12", wards2$date);
wards2$date <- as.Date(wards2$date);
wards2$wardboth <- paste(stringr::str_pad(wards2$wardnum, 2, pad="0"), wards2$wardname);
wards2$wardboth <- forcats::fct_rev(factor(wards2$wardboth));
ggplot(wards2 %>% filter(date >= as.Date('2020-06-09') + 7)) + geom_tile(aes(fill=value, x=date, y=wardboth)) +
  theme_minimal() +
  scale_fill_fermenter(palette='GnBu', direction=1, breaks=c(1:4*25,150,200,250,300), oob=scales::squish) +
  scale_x_date(breaks='1 month', date_labels='%b') +
  theme(axis.title.x = element_blank(), axis.title.y=element_blank()) +
  ggtitle('Ottawa-Gatineau COVID-19 weekly case rates') +
  labs(fill='Cases/100K',
       caption='Ottawa: excl. LTC/RHs; Gatineau: incl. CSHLD/RI/RPAs') +
ggsave('wardsHeat.png', width=4, height=4, dpi='print', device='png', units='in',scale=2);

setupPlot <- function(var) {
  ggplot(wards) +
    scale_fill_distiller(palette='Blues', direction=1,) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank()) +
    geom_sf_text(aes(label=WARD_NUM));
}
#cowplot::plot_grid(
#  setupPlot() + geom_sf(aes(fill=d_09.Jun.20)) + theme(legend.position='none'),
#  setupPlot() + geom_sf(aes(fill=d_06.Jul.20)) + theme(legend.position='none'),
#  setupPlot() + geom_sf(aes(fill=d_20.Jul.20)),
#  nrow=1
#);


#ages <- read.csv('../input/COVID-19_Weekly_Cases_and_Rates_by_Age_in_Ottawa__Last_6_Weeks_.csv', fileEncoding='macintosh');
#colnames(ages) <- c(
#  'week',
#  'rate_0-19',   # by episode date
#  'rate_20-39',
#  'rate_40-59',
  #'rate_60-79',
 # 'rate_80+',
 # 'count_episode',
#  'count_report',
#  'rate_report',
  #'count_0-19',
 # 'count_20-39',
#  'count_40-59',
#  'count_60-79',
 # 'count_80+',
#  'OBJECTID'
#);
#ages$week <- as.Date(as.character(ages$week));

rates <- read.csv('../input/COVID-19_Cases_and_Deaths_in_Ottawa.csv', fileEncoding='macintosh');
rates <- rates[,c(1,16:25)];
colnames(rates) <- c(
  'date',
  'cumrate_0-9',   # by episode date
  'cumrate_10-19',
  'cumrate_20-29',
  'cumrate_30-39',
  'cumrate_40-49',
  'cumrate_50-59',
  'cumrate_60-69',
  'cumrate_70-79',
  'cumrate_80-89',
  'cumrate_90+');
rates$date <- as.Date(as.character(rates$date));
temp <- rates[,2:11] - rbind(0 * rates[1,2:11], rates[1:(nrow(rates)-1),2:11])
colnames(temp) <- gsub('cum', '', colnames(rates)[2:11]);
rates <- cbind(date = rates$date, temp);

popCount <- list(
  '0-9' = 48870+53715,
  '10-19' = 53095+57190,
  '20-29' = 68645+63695,
  '30-39' = 61670+59575,
  '40-49' = 62710+65960,
  '50-59' = 73210+66765,
  '60-69' = 54990+48130,
  '70-79' = 33875+24600,
  '80-89' = 17950+11720,
  '90+' = 5995+1640+225
);
ages <- rates;
ages$'rate_0-9' = ages$'rate_0-9' * popCount$'0-9';
ages$'rate_10-19' = ages$'rate_10-19' * popCount$'10-19';
ages$'rate_20-29' = ages$'rate_20-29' * popCount$'20-29';
ages$'rate_30-39' = ages$'rate_30-39' * popCount$'30-39';
ages$'rate_40-49' = ages$'rate_40-49' * popCount$'40-49';
ages$'rate_50-59' = ages$'rate_50-59' * popCount$'50-59';
ages$'rate_60-69' = ages$'rate_60-69' * popCount$'60-69';
ages$'rate_70-79' = ages$'rate_70-79' * popCount$'70-79';
ages$'rate_80-89' = ages$'rate_80-89' * popCount$'80-89';
ages$'rate_90+' = ages$'rate_90+' * popCount$'90+';
ages[,2:11] <- ages[,2:11] / 100000 * 1095134/934243 * 0.97;
colnames(ages)[2:11] <- gsub('rate_', 'cases_', colnames(ages)[2:11]);

#ages <- rates %>% across::across(all_of(names(popCount)), ~.x * popCount[[cur_column()]]);

rates <- rates %>% tidyr::pivot_longer(cols=2:11, names_to='age', names_prefix='rate_');
ages <- ages %>% tidyr::pivot_longer(cols=2:11, names_to='age', names_prefix='cases_');

rates$week <- floor((rates$date - as.Date('2020-01-06'))/7);
ages$week <- floor((ages$date - as.Date('2020-01-06'))/7);
ggplot(rates %>% group_by(week, age) %>% summarise(date=min(date), cases=sum(value))
         %>% filter(date > as.Date('2020-07-01') & date < as.Date('2020-12-30') & !(age %in% c('80-89', '90+'))), aes(y=age, x=date)) +
  geom_tile(aes(fill=cases)) +
  scale_fill_fermenter(palette='Blues', direction=1, breaks = 1:8*10, oob=scales::squish) +
  labs(fill='weekly cases/100K', x='episode date') +
#  geom_text(aes(label=ifelse(cases > 200,round(cases, -1),'')), size=2) +
  scale_x_date(breaks='2 weeks', date_labels='%b %d') +
  ggtitle('Ottawa COVID-19 Case Rates By Age');


agesFilter <- ages %>% group_by(week, age) %>%
  summarise(date=min(date), cases=sum(value)) %>%
  filter(date > as.Date('2020-07-01') & date < as.Date('2020-12-25'));
agesFilter$age <- forcats::fct_rev(agesFilter$age);
#agesFilterYoung <- agesFilter %>% filter(!age %in% c('70-79','80-89','90+'));
agesFilterYoung <- agesFilter;
agesFilterYoung$cases[agesFilter$age %in% c('80-89','90+')] <- 0;
p1 <- ggplot(agesFilter, aes(x=date, y=cases)) +
  geom_bar(aes(fill=age), position='stack', stat='identity') +
  labs(y='cases', x='episode date') +
  scale_fill_brewer(palette='PRGn', direction=-1);
p2 <- ggplot(agesFilter, aes(x=date, y=cases)) +
  geom_bar(aes(fill=age), position='fill', stat='identity') +
  labs(y='cases', x='episode date') +
  scale_fill_brewer(palette='PRGn', direction=-1) +
  scale_y_continuous(labels=scales::percent);
p3 <- ggplot(agesFilterYoung, aes(x=date, y=cases)) +
  geom_bar(aes(fill=age), position='stack', stat='identity') +
  labs(y='cases (excl. age 80+)', x='episode date') +
  scale_fill_brewer(palette='PRGn', direction=-1);
agesFilterYoung <- agesFilterYoung %>% group_by(date) %>%
    mutate(per = prop.table(cases) * 100);
p4 <- ggplot(agesFilterYoung, aes(x=date, y=cases)) +
  geom_bar(aes(fill=age), position='fill', stat='identity') +
  labs(y='cases (excl. age 80+)', x='episode date') +
  geom_text(aes(label=paste0(round(per), '%'), alpha=age),
            position=position_fill(vjust=0.5)) +
  scale_fill_brewer(palette='PRGn', direction=-1) +
  scale_y_continuous(labels=scales::percent) +
  scale_alpha_manual(values=c(0,0,0,0,0,0,0,1,0,0));
#  ggtitle('COVID-19 Age Breakdown in Ottawa');
gridExtra::grid.arrange(p1,p2,p3,p4, nrow=2, top='COVID-19 Age Breakdown in Ottawa')

#  foo <- read.csv('../input/ottawa_age.csv');
#  names(foo)[1] <- 'date';
#  foo$date <- as.Date(foo$date);
#foo <- foo %>% tidyr::pivot_longer(names_to='age', cols=2:ncol(foo), names_prefix='X');
#  ggplot(foo, aes(y=age, x=date)) + geom_tile(aes(fill=value)) + scale_fill_distiller(palette='YlGnBu') + labs(fill='weekly\ncases/100k') + ggtitle('Ottawa COVID-19 Case Rates by Age') + scale_x_date(breaks='2 weeks', date_labels='%b %d')

