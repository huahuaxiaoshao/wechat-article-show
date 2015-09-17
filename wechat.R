require("XML")
final_account<-c()
final_title_img<-c()
final_article_link<-c()
final_user_name<-c()
final_source_link<-c()
final_article_title<-c()
final_circle_desc<-c()
final_qc_code<-c()
final_date<-c()
final_original<-c()
final_writer<-c()
final_true_link<-c()
open_browser<-remoteDriver(browserName="firefox")
open_browser$open()
get_public_account<-function(){
  keyword<<-edit("请输入关键词")
  content_type<<-edit("请输入要搜索的内容类型，分公众号和文章，并用双引号括起。")
  time_range<<-edit("请输入需要的时间氛围，分全部时间，一天内，一周内，一月内，一年内")
  time_sheet<<-data.frame(time=c("全部时间","一天内","一周内","一月内","一年内"),value=c(0,1,2,3,4))
  time_range<<-time_sheet[time_sheet[1]==time_range,2]
  if(keyword==""){
    warning("搜索内容不能为空！")
  }else if(content_type!="公众号"&content_type!="文章"){
    warning("搜索类型不正确！")
  }else{
    message("Congratulations！你可以调用get_content函数抓取内容了。")
  }
}
get_content<-function(){
  if(content_type=="公众号"){
    target_link<-paste("http://weixin.sogou.com/weixin?type=1","&query=",keyword,"&tsn=",time_range,sep="")
    target_content<-xmlRoot(htmlParse(target_link))
  }else if(content_type=="文章"){
    #获取1-10页内容
    for(j in 1:10){
      #从搜索结果页获取文章链接，文章小图片等数据
      target_link<-URLencode(paste("http://weixin.sogou.com/weixin?type=2","&query=",keyword,"&tsn=",time_range,"&page=",j,sep=""))
      open_browser$navigate(target_link)
      target_content<-xmlRoot(htmlParse(open_browser$getPageSource()[[1]]))
      article_wechat_account<-xpathSApply(target_content,"//a[@id='weixin_account']")
      article_link<-xpathSApply(target_content,"//a[starts-with(@id,'sogou_vr')]")
      article_title_img<-xpathSApply(target_content,"//a[starts-with(@id,'sogou_vr')]/img")
      for(i in 1:length(article_wechat_account)){
        tem_account<-iconv(xmlGetAttr(article_wechat_account[[i]],"title"),"utf-8","gbk")
        tem_title_img<-xmlGetAttr(article_title_img[[i]],"src")
        tem_article_link<-xmlGetAttr(article_link[[i+1]],"href")
        final_account<-append(final_account,tem_account)
        final_title_img<-append(final_title_img,tem_title_img)
        final_article_link<<-append(final_article_link,tem_article_link)
      }
    }
    for(k in 1:length(final_article_link)){
      rd_link<-paste("http://weixin.sogou.com",final_article_link[k],sep="")
      open_browser$navigate(rd_link)
      Sys.sleep(5)
      tem_true_link<-open_browser$getCurrentUrl()[[1]]
      final_true_link<-append(final_true_link,tem_true_link)
    }
    final_article_link<-final_true_link
    for(i in 1:length(final_article_link)){
        base_content<-xmlRoot(htmlParse(final_article_link[i]))
        tem_em<-xpathSApply(base_content,"//em",xmlValue)
        if(length(tem_em)==2){
          tem_writer<-tem_em[2]
          tem_date<-tem_em[1]
        }else{
          tem_writer<-"无作者"
          tem_date<-tem_em[1]
        }
        base_script_content<-xpathSApply(base_content,"//script")
        main_js_info<-xmlValue(base_script_content[[length(base_script_content)-1]])
        qc_code<-xmlValue(base_script_content[[1]])
        main_js_info<-gsub("&amp;","&",main_js_info)
        main_js_info<-gsub(" ","",main_js_info)
        main_js_info<-gsub("\"","",main_js_info)
        main_js_info<-gsub("\r\n","",main_js_info)
        main_js_info<-gsub("&nbsp;"," ",main_js_info)
        main_js_info<-strsplit(main_js_info,";")
        qc_code<-gsub("&amp;","&",qc_code)
        qc_code<-gsub(" ","",qc_code)
        qc_code<-gsub("\"","",qc_code)
        qc_code<-gsub("\r\n","",qc_code)
        qc_code<-gsub("&nbsp;"," ",qc_code)
        qc_code<-strsplit(qc_code,";")
        tem_user_name<-substring(main_js_info[[1]][12],14,nchar(main_js_info[[1]][12]))#公众号初始id
        tem_source_link<-substring(main_js_info[[1]][22],19,nchar(main_js_info[[1]][22]))#原文链接
        tem_source_link<-gsub("'","",tem_source_link)
        tem_article_title<-substring(main_js_info[[1]][17],14,nchar(main_js_info[[1]][17]))#文章标题
        tem_circle_desc<-substring(main_js_info[[1]][18],13,nchar(main_js_info[[1]][18]))#朋友圈描述
        tem_biz<-substring(qc_code[[1]][4],8,nchar(qc_code[[1]][4]))#获取biz参数，用于串成公众号二维码链接
        tem_qc_code<-paste("http://mp.weixin.qq.com/mp/qrcode?scene=10000001&size=102&__biz=",tem_biz,sep="")#默认102*102的尺寸
        final_user_name<-append(final_user_name,tem_user_name)
        final_source_link<-append(final_source_link,tem_source_link)
        final_article_title<-append(final_article_title,tem_article_title)
        final_circle_desc<-append(final_circle_desc,tem_circle_desc)
        final_qc_code<-append(final_qc_code,tem_qc_code)
        final_date<-append(final_date,tem_date)
        is_original<-xpathSApply(base_content,"//span[@id='copyright_logo']",xmlValue)
        if(class(is_original)=="character"){
          is_original<-"Y"
        }else{
          is_original<-"N"
        }
        final_original<-append(final_original,is_original)
        final_writer<-append(final_writer,tem_writer)
    }
    final_sheet<<-data.frame(final_account,final_article_link,final_title_img,final_user_name,final_source_link,final_article_title,final_circle_desc,final_qc_code,final_date,final_original,final_writer)
    colnames(final_sheet)<-c("公众号名称","文章链接","文章配图","公众号初始id","原文链接","文章标题","朋友圈描述","公众号二维码","日期","是否原创","文章作者")
    write.csv(final_sheet,paste("data/",keyword,"_",unlist(strsplit(date()," "))[5],"_",unlist(strsplit(date()," "))[2],"_",unlist(strsplit(date()," "))[3],substring(unlist(strsplit(date()," "))[4],1,2),"_rawdata.csv",sep=""),row.names = F)
  }else{
    return(0)
  }
}