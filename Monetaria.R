library(ipeadatar)
library(tidyverse)
library(magrittr)
library(rbcb)
library(gganotate)

ipeadatar::available_series() %>% View()

selic = rbcb::get_series(432, start_date = "2000-01-01" %>% as.Date())

colnames(selic) = c('date', 'value')

selic %>% group_by(mes = month(date), ano = year(date)) %>%
  summarise(date = date, Selic = mean(value, na.rm = T))%>%
  ungroup() %>%
  select(date, Selic)

dados = ipeadata('BM12_IPCAEXP612')

dados %<>% dplyr::select(date, value)

dados %<>% left_join(selic, by = 'date')

colnames(dados) = c('date', 'Expectativa IPCA', 'Selic')


dados %<>% arrange(date)

to_plot = dados %>% pivot_longer(cols = -date, names_to = 'Serie', values_to = 'Values')

selic = to_plot %>% dplyr::filter(Serie == "Selic")
expec = to_plot %>% dplyr::filter(Serie == "Expectativa IPCA")


plot_series = to_plot %>% ggplot(aes(x = date, y = Values, color = Serie))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  geom_text(x = to_plot$date %>% as.Date() %>% last + 40,
            y = selic$Values %>% last + 1.4,
            label = selic$Values %>% last, col = "black")+
  geom_text(x = to_plot$date %>% as.Date() %>% last + 40,
            y = expec$Values %>% last + 1.4,
            label = expec$Values %>% last %>% round(2), col = "black")+
  labs(title = 'Observando a Similaridade das duas séries',x = NULL, y = NULL, colour = "Serie")+
  bbplot::bbc_style()+
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        plot.title = element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))


df = forecast::ma(dados, order = 6, centre= F) %>% as.data.frame()

dados_df = cbind(dados$date, df[,-1])
colnames(dados_df) = c("date", "MM6 Expectativa IPCA", "MM6 Selic")


to_plot_smooth = dados_df %>% pivot_longer(cols = -date,
                                           names_to = "Serie",
                                           values_to = "Values")

smooth = to_plot_smooth %>% ggplot(aes(x = date, y = Values, color = Serie))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  labs(caption = 'Banco Central e IPEA',
       title = 'MM6',
  x = NULL,
  y = NULL)+
  bbplot::bbc_style()+
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        plot.title = element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))

library(ggpubr)

gplot_f = ggpubr::ggarrange(plot_series, smooth,
                  common.legend = T,
                  ncol = 1,
                  legend = "bottom")

