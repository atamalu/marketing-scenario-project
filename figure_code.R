### Experiment Description: timeline
rm(list = ls())

library(ggplot2)
theme_set(theme_classic())
df <- read.csv('milestones.csv')
df <- data.frame(
  mydate = c(1:30, 31:60, 61:90),
  month = c(
    rep('Month 1', 30),
    rep('Month 2', 30),
    rep('Month 3', 30)
  ),
  when.event = c(
    c(1, rep(NA, 29)),
    c(31, rep(NA, 29)),
    c(61, rep(NA, 29))
      ),
  position = c(
    rep(0.25, 30),
    rep(-0.25, 30),
    rep(0.25, 30)
  )
)

event.df <- data.frame(
  Day = c(1, 31, 61),
  Position = c(0.25, -0.25, 0.25),
  Label = c('Survey', 'Survey', 'Survey')
)

rect.df <- data.frame(xmin = 1, xmax = 30,
                      ymin = -0.25, ymax = 0.25)

timeline_plot <- ggplot(df, aes(y = 0)) +
  geom_rect(data=rect.df,
            aes(xmin=xmin, xmax=xmax,
                ymin=ymin, ymax=ymax),
            alpha = 0.2) +
  # add middle line
  geom_hline(yintercept = 0,
             color= 'black', size = 0.3) +
  # add segments to each event
  geom_segment(data = df[!is.na(df$when.event),],
               aes(x = mydate, y = position, yend = 0,
                   xend = mydate)) +
  geom_text(data = event.df,
            aes(x = Day, y = Position, label = Label),
            size=2.5)

timeline_plot +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom"
  )


df <- data.frame(
  event = c('Survey (baseline)', 'Survey 1', 'Survey 2', 'Survey 3'),
  event.date = c(1, 31, 61, 91),
  y.pos = c(20, -20, 20, -20)
)

df$y.abovebelow <- ifelse(df$y.pos >= 0, 3, 1)

### blank plot
plot(x = 0:91, y = c(-45:46),
     xlab = '', ylab = '',
     type = 'n', bty = 'n',
     axes = FALSE)

### draw background rectangles
rect1.col <- alpha('red', 0.1)
rect2.col <- alpha('blue', 0.1)
rect3.col <- alpha('green', 0.1)

rect(xleft = df$event.date[1], xright = df$event.date[2],
     ybottom = min(df$y.pos), ytop = max(df$y.pos),
     col = rect1.col, border=NA)
rect(xleft = df$event.date[2], xright = df$event.date[3],
     ybottom = min(df$y.pos), ytop = max(df$y.pos),
     col = rect2.col, border = NA)
rect(xleft = df$event.date[3], xright = df$event.date[4],
     ybottom = min(df$y.pos), ytop = max(df$y.pos),
     col = rect3.col, border = NA)

### add lines
abline(h = 0) # midline

segments(x0 = df$event.date,
         y0 = rep(0, length(df$y.pos)), y1 = df$y.pos) # add lines to events
text(x = df$event.date + 1, y = df$y.pos,
     pos = df$y.abovebelow,
     labels = df$event)

draw_event <- function(x, yend, ystart = 0){
  lines(x = )
}