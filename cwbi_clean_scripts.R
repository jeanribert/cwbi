### CWBI clean scripts ###

library(pacman)
p_load(tidyverse, sf, patchwork, raster)

####################
### Get the data ###
####################

CWBI <- readRDS("cwbi_clean_data.rds") %>% dplyr::filter(!CWBI>5) # Remove outlier
CWBI <- CWBI %>% st_as_sf()
CWBI <- st_transform(CWBI, crs = 5070)
counties <- readRDS("coterm_cty_sf.RDS")
frr <- readRDS("FRR.rds") %>% st_as_sf()
frr <- st_transform(frr, crs = crs(CWBI))
states <- readRDS("states_sf.RDS") %>% filter(STATE_FIPS %in% counties$STATEFP)
states <- st_transform(states, crs = crs(CWBI))

###########################################
### Mapping AEC distribution (Figure 1) ###
###########################################

ggplot() +
  geom_sf(data = CWBI %>% filter(job_or_gdp=="All AEC Counties"), aes(fill = job_or_gdp), fill = "black") +
  geom_sf(data = CWBI %>% filter(job_or_gdp=="non-AEC Counties"), aes(fill = job_or_gdp), fill = "white") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.border=element_blank(),
        panel.grid.major = element_line(colour = "transparent"))


###############################
### Mapping CWBI (Figure 2) ###
###############################

my_color <- c("< -1.0 Std. Dev." = "#EE8866", 
              "-1.0 - -0.5 Std. Dev." = "#EEDD88",
              "-0.5 - 0.5 Std. Dev." = "#77AADD", 
              "0.5 - 1.0 Std. Dev." = "#BBCC33",
              "> 1 Std. Dev." = "#AAAA00")


ggplot() +
  geom_sf(data = CWBI, aes(fill = CWBI_Levels), color = "transparent") +
  geom_sf(data = states, fill = "transparent", color = "white") +
  scale_fill_manual(values = my_color) +
  theme(axis.text = element_blank(),
        plot.margin = margin(1, 1, 0.3, 0.5, "cm"),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.text=element_text(size=8),
        legend.position = c(0.11, 0.21),
        legend.background = element_blank()) +
  guides(fill=guide_legend(title="CWBI"))

##########################
### Boxplot (Figure 3) ###
##########################

bp <- CWBI %>% 
  as.data.frame() %>% 
  dplyr::select(GEOID, job_or_gdp, gdp_only, job_only, CWBI, Metro_Status)

bpall <- bp %>% 
  dplyr::select(GEOID, job_or_gdp, CWBI, Metro_Status) %>% 
  rename(AEC_Type = 'job_or_gdp')

bpjob <- bp %>% 
  dplyr::select(GEOID, job_only, CWBI, Metro_Status) %>% 
  rename(AEC_Type = 'job_only') %>% 
  filter(AEC_Type == "AEC by job only")

bpgdp <- bp %>% 
  dplyr::select(GEOID, gdp_only, CWBI, Metro_Status) %>% 
  rename(AEC_Type = 'gdp_only') %>% 
  filter(AEC_Type == "AEC by GDP only")

bpcomb <- bind_rows(bpall, bpjob, bpgdp)

bp_color <- c("All AEC Counties" = "#33a02c", 
              "AEC by GDP only" = "#cab2d6",
              "AEC by job only" = "#1f78b4", 
              "non-AEC Counties" = "#a6cee3")

# plotting the data
bpcomb %>%
  ggplot() +
  geom_boxplot(aes(x = Metro_Status, y= CWBI, fill = AEC_Type), 
               width = 0.9/length(unique(bpcomb$Metro_Status)), alpha = 0.5) +
  geom_hline(yintercept = mean(bpcomb$CWBI, na.rm = T), color="darkgrey", linetype = "dashed") +
  xlab("") +
  ylab("") +
  guides(alpha = guide_legend(override.aes = list(fill = "steelblue")),
         fill=guide_legend(title="County type")) +
  theme_classic() +
  scale_fill_manual(values = bp_color) +
  scale_y_continuous(breaks = c(-5, -1, -0.5, 0.5, 1, 5),
                     labels = c(-5, -1, -0.5, 0.5, 1, 5)) +
  theme(legend.position = c(0.12, 0.82),
        legend.box="vertical",
        legend.background = element_rect(colour = "black", fill=NA),
        panel.border=element_blank(),
        panel.grid = element_line(colour = "transparent"),
        axis.text.x = element_text(size = 12, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 12, vjust = 0.5, hjust=0.5))

############################################################################
### Mapping AEC distribution based on GDP and Job separately (Figure S1) ###
############################################################################

ggplot() +
  geom_sf(data = CWBI %>% filter(gdp_only=="AEC by GDP only"), aes(fill = gdp_only), fill = "black") +
  geom_sf(data = CWBI %>% filter(gdp_only=="non-AEC"), aes(fill = gdp_only), fill = "white") +
  theme_minimal() +
  labs(title = "AEC by GDP only") +
  theme(axis.text = element_blank(),
        panel.border=element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

ggplot() +
  geom_sf(data = CWBI %>% filter(job_only=="AEC by job only"), aes(fill = job_only), fill = "black") +
  geom_sf(data = CWBI %>% filter(job_only=="non-AEC"), aes(fill = job_only), fill = "white") +
  theme_minimal() +
  labs(title = "AEC by job only") +
  theme(axis.text = element_blank(),
        panel.border=element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

######################################
### Density plots (Figure S2) ###
######################################

# AEC versus non-AEC
d1 <- ggplot(CWBI, aes(x = CWBI)) + 
  geom_density(aes(group = job_or_gdp, fill = job_or_gdp), 
               alpha = 0.5, show.legend = F) +
  scale_fill_manual(values = c("#33a02c", "#a6cee3"),
                    labels = c("AEC", "non-AEC"),
                    name = "AEC Type") +
  theme_classic() +
  labs(title = "a") +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 8, vjust = 0.5, hjust=0.5))

# Rural versus Urban
d2 <- ggplot(CWBI, aes(x = CWBI)) + 
  geom_density(aes(group = Metro_Status, fill = Metro_Status), 
               alpha = 0.5) +
  scale_fill_manual(values = c("#A3A500", "#C77CFF"),
                    name = "Rural status") +
  theme_classic() +
  labs(title = "d") +
  xlab("") +
  ylab("") +
  theme(legend.box="vertical",
        legend.position = "right",
        legend.text = element_text(size = 9),
        axis.text.x = element_text(size = 9, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 8, vjust = 0.5, hjust=0.5))

# Urban AEC versus Urban non-AEC
d3 <- ggplot(CWBI %>% filter(Metro_Status == "Urban"), aes(x = CWBI)) + 
  geom_density(aes(group = job_or_gdp, fill = job_or_gdp), 
               alpha = 0.5, show.legend = F) +
  scale_fill_manual(values = c("#33a02c", "#a6cee3"),
                    labels = c("AEC", "non-AEC"),
                    name = "AEC Type") +
  theme_classic() +
  labs(title = "b") +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 8, vjust = 0.5, hjust=0.5))

# Rural AEC versus Rural non-AEC
d4 <- ggplot(CWBI %>% filter(Metro_Status == "Rural"), aes(x = CWBI)) + 
  geom_density(aes(group = job_or_gdp, fill = job_or_gdp), 
               alpha = 0.5) +
  scale_fill_manual(values = c("#33a02c", "#a6cee3"),
                    labels = c("AEC", "non-AEC"),
                    name = "AEC Type") +
  theme_classic() +
  labs(title = "c") +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 8, vjust = 0.5, hjust=0.5))

(d1 + d4) / (d3 + d2)

###########################################
### Boxplots for comparison (Figure S3) ###
###########################################

# CWBI by metro status and disaggregated AEC
p1 <- bpcomb %>% 
  ggplot() +
  geom_boxplot(aes(x = Metro_Status, y= CWBI, fill = AEC_Type),
               width = 0.9/length(unique(bpall$Metro_Status)), alpha = 0.5) +
  geom_hline(yintercept = mean(bpall$CWBI, na.rm = T), color="darkgrey", linetype = "dashed") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_fill_manual(values = bp_color,
                    name = "AEC Type") +
  ggtitle("d") +
  scale_y_continuous(breaks = c(-5, -1, -0.5, 0.5, 1, 5),
                     labels = c(-5, -1, -0.5, 0.5, 1, 5)) +
  theme(legend.box="vertical",
        legend.position = "right",
        legend.text = element_text(size = 9),
        panel.border=element_blank(),
        panel.grid = element_line(colour = "transparent"),
        axis.text.x = element_text(size = 9, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 8, vjust = 0.5, hjust=0.5))

# CWBI by metro status
p2 <- bpall %>% 
  ggplot() +
  geom_boxplot(aes(x = Metro_Status, y= CWBI, fill = Metro_Status), show.legend = F,
               width = 0.5/length(unique(bpall$Metro_Status)), alpha = 0.5) +
  geom_hline(yintercept = mean(bpall$CWBI, na.rm = T), color="darkgrey", linetype = "dashed") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_fill_manual(values = c("#A3A500", "#C77CFF"),
                    name = "Rural status") +
  ggtitle("a") +
  scale_y_continuous(breaks = c(-5, -1, -0.5, 0.5, 1, 5),
                     labels = c(-5, -1, -0.5, 0.5, 1, 5)) +
  theme(panel.border=element_blank(),
        panel.grid = element_line(colour = "transparent"),
        axis.text.x = element_text(size = 9, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 8, vjust = 0.5, hjust=0.5))

# CWBI by  AEC
p3 <- bpall %>% 
  ggplot() +
  geom_boxplot(aes(x = AEC_Type, y= CWBI, fill = AEC_Type), show.legend = F,
               width = 0.5/length(unique(bpall$AEC_Type)), alpha = 0.5) +
  geom_hline(yintercept = mean(bpall$CWBI, na.rm = T), color="darkgrey", linetype = "dashed") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_fill_manual(values = c("#33a02c", "#a6cee3")) +
  ggtitle("b") +
  scale_y_continuous(breaks = c(-5, -1, -0.5, 0.5, 1, 5),
                     labels = c(-5, -1, -0.5, 0.5, 1, 5)) +
  theme(panel.border=element_blank(),
        panel.grid = element_line(colour = "transparent"),
        axis.text.x = element_text(size = 9, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 8, vjust = 0.5, hjust=0.5))

# CWBI by  AEC & by metro
p4 <- bpall %>% 
  ggplot() +
  geom_boxplot(aes(x = Metro_Status, y= CWBI, fill = AEC_Type), show.legend = F,
               width = 0.5/length(unique(bpall$Metro_Status)), alpha = 0.5) +
  geom_hline(yintercept = mean(bpall$CWBI, na.rm = T), color="darkgrey", linetype = "dashed") +
  xlab("") +
  ylab("") +
  guides(alpha = guide_legend(override.aes = list(fill = "steelblue")),
         fill=guide_legend(title="")) +
  theme_classic() +
  scale_fill_manual(values = c("#33a02c", "#a6cee3")) +
  ggtitle("c") +
  scale_y_continuous(breaks = c(-5, -1, -0.5, 0.5, 1, 5),
                     labels = c(-5, -1, -0.5, 0.5, 1, 5)) +
  theme(legend.box="vertical",
        legend.position = "right",
        legend.text = element_text(size = 9),
        panel.border=element_blank(),
        panel.grid = element_line(colour = "transparent"),
        axis.text.x = element_text(size = 9, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size = 8, vjust = 0.5, hjust=0.5))

(p2 + p4) / (p3 + p1)

