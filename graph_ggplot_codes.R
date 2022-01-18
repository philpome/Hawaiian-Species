#mel_muc_113018
positions<-c("PVS2", "LN")
ggplot(mel_muc_DV, aes(x=Treatment, y=Survived, label=sprintf("%0.2f", round(Survived, digits = 2)))) + 
    geom_bar(stat="identity", fill=c("#4DBBD5FF", "#F39B7FFF")) + 
    scale_x_discrete(limits = positions) + 
    geom_text(size = 6, position = position_dodge(0.9))

#mel_muc_011019
ggplot(mel_muc_precult, aes(fill=LN, y=Survived, x=PVS, label=sprintf("%0.2f", round(Survived, digits = 2)))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("#4DBBD5FF", "#F39B7FFF")) +
  facet_wrap(~Precult) + 
  geom_text(size = 5, position = position_dodge(0.9))

#cyr_gra_102618
positions = c("LN", "PVS2", "Isolation")
ggplot(cyr_gra_tips, aes(x=Treatment, y=Survived, label=sprintf("%0.2f", round(Survived, digits = 2)))) +
    geom_bar(stat="identity", fill=c("#4DBBD5FF", "#E64B35FF", "#F39B7FFF")) +
    scale_x_discrete(limits = positions) +
    geom_text(size = 6, position = position_dodge(0.9))

#cyr_gra_122818
ggplot(cyr_gra_pvs, aes(fill=LN, y=Survived, x=PVS, label=sprintf("%0.2f", round(Survived, digits = 2)))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("#4DBBD5FF", "#F39B7FFF")) +
  geom_text(size = 5, position = position_dodge(0.9))

#cyr_gra_022819
ggplot(cyr_gra_disk, aes(fill=LN, y=Survived, x=Size, label=sprintf("%0.2f", round(Survived, digits = 2)))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("#4DBBD5FF", "#F39B7FFF", "#E64B35FF")) +
  facet_wrap(~Dev) + 
  geom_text(size = 5, position = position_dodge(0.9))