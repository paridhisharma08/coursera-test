clc;
clear all;
close all;
N=2;%number of days
count=zeros(N,3);
for j=1:N
    [num, txt, raw] =xlsread(sprintf('londonanalysis%d1.xlsx',j));
    for i= 1:length(txt(:,2)) -1
        if(strcmp(txt(i+1,2),'positive'))
            count(j,1)=count(j,1)+1;
        elseif(strcmp(txt(i+1,2),'neutral'))
            count(j,2)=count(j,2)+1;
        elseif(strcmp(txt(i+1,2),'negative'))
            count(j,2)=count(j,2)+1;
        end
    end
    
end

b=bar(count, 'stacked');
mycolor=[0 1 0;1 0 0;0 0 1];% u can change this to change colours
colormap(mycolor)
    
for i=1:N
    if(count(i,1)~=0)
            text(i,round(count(i,1)/2),num2str(count(i,1)),'FontSize',12);%u can change font size
    end
    if(count(i,2)~=0)
            text(i,floor(count(i,1)+count(i,2)/2),num2str(count(i,2)),'FontSize',12);
    end
    if(count(i,3)~=0)
            text(i,floor(count(i,1)+count(i,2)+count(i,3)/2),num2str(count(i,3)),'FontSize',12);
    end
        text(i,count(i,1)+count(i,2)+count(i,3)+5,num2str(sum(count(i,:))),'FontSize',12);
end
    
set(gca,'XTickLabel',{'duda','komorowski'})%add more days
legend('positive', 'neutral', 'negative','Location','NorthEastOutside')
%x = -pi:pi/20:pi; y1 = sin(x); y2 = cos(x); figure plot(x,y1,'-ro',x,y2,'-.b') legend('sin(x)','cos(x)')
ylim([0 max(max(count))+20])