#install.packages("rjson")
#install.packages("wordcloud")
#packages <- c("rjson","wordcloud")
#require(packages)


library("rjson")
library("wordcloud")

rm(list = ls())
### Data Directory ###
Data_directory <- ''
##########

word_cloud <- function(words,freq){
  set.seed(1234)
  wordcloud(words = words, freq = freq, min.freq = 1,scale = c(100/length(words),5*100/length(words)),
            max.words=300, random.order=TRUE, rot.per=0, random.color = TRUE,
            colors=brewer.pal(8, "Dark2"),use.r.layout=TRUE,fixed.asp = TRUE)
  
}

setwd(Data_directory)
Profile <- fromJSON(paste(readLines(paste0(Data_directory,'\\profile_information\\profile_information.json'),warn=FALSE), collapse=""))
User_Name <- Profile$profile$name$full_name

ads_interests <- fromJSON(paste(readLines(paste0(Data_directory,'\\ads\\ads_interests.json'),warn=FALSE), collapse=""))

word_cloud(words = ads_interests$topics, freq = lengths(ads_interests$topics))


name <- colnames(as.data.frame(ads_interests))[1]

total_topics <- length(ads_interests[[name]])

advertisers_who_uploaded_a_contact_list_with_your_information <- fromJSON(paste(readLines(paste0(Data_directory,'\\ads\\advertisers_who_uploaded_a_contact_list_with_your_information.json'),warn=FALSE), collapse=""))

word_cloud(words = advertisers_who_uploaded_a_contact_list_with_your_information$custom_audiences, freq = lengths(advertisers_who_uploaded_a_contact_list_with_your_information$custom_audiences))

name <- colnames(as.data.frame(advertisers_who_uploaded_a_contact_list_with_your_information))[1]

total_custom_audiences <- length(advertisers_who_uploaded_a_contact_list_with_your_information[[name]])

apps_and_websites <- fromJSON(paste(readLines(paste0(Data_directory,'\\apps_and_websites\\apps_and_websites.json'),warn=FALSE), collapse=""))

total_apps_and_websites <- length(apps_and_websites$installed_apps)

posts_from_apps_and_websites <- fromJSON(paste(readLines(paste0(Data_directory,'\\apps_and_websites\\posts_from_apps_and_websites.json'),warn=FALSE), collapse=""))

total_posts_from_apps_and_websites <- length(posts_from_apps_and_websites$app_posts)

total <- 0

for(i in 1:total_posts_from_apps_and_websites){
if(length(grep(User_Name,posts_from_apps_and_websites$app_posts[[i]]$title,ignore.case = TRUE))!=0){
  total <- total + 1 
  }
}
total_posts_from_apps_and_websites_user <- total

comments <- fromJSON(paste(readLines(paste0(Data_directory,'\\comments\\comments.json'),warn=FALSE), collapse=""))

total_comments <- length(comments$comments)

total <- 0
total_attachments <- 0

for(i in 1:total_comments){
  if(length(grep(User_Name,comments$comments[[i]]$data[[1]]$comment$author,ignore.case = TRUE))!=0){
    total <- total + 1 
  }
  if(fine_length <-length(comments$comments[[i]]$attachments)!=0){
    total_attachments <- fine_length + total_attachments
  }
}
total_comments_user <- total
 

event_invitations <- fromJSON(paste(readLines(paste0(Data_directory,'\\events\\event_invitations.json'),warn=FALSE), collapse=""))

events_invited <- length(event_invitations$events_invited)


your_event_responses <- fromJSON(paste(readLines(paste0(Data_directory,'\\events\\your_event_responses.json'),warn=FALSE), collapse=""))

events_joined <- length(your_event_responses$event_responses$events_joined)
events_declined <- length(your_event_responses$event_responses$events_declined)
events_interested <- length(your_event_responses$event_responses$events_interested)

friends <- fromJSON(paste(readLines(paste0(Data_directory,'\\friends\\friends.json'),warn=FALSE), collapse=""))

total_friends <- length(friends$friends)

received_friend_requests <- fromJSON(paste(readLines(paste0(Data_directory,'\\friends\\received_friend_requests.json'),warn=FALSE), collapse=""))

total_received_friend_requests <- length(received_friend_requests$received_requests)


rejected_friend_requests <- fromJSON(paste(readLines(paste0(Data_directory,'\\friends\\rejected_friend_requests.json'),warn=FALSE), collapse=""))

total_rejected_friend_requests <- length(rejected_friend_requests$rejected_requests)

removed_friends <- fromJSON(paste(readLines(paste0(Data_directory,'\\friends\\removed_friends.json'),warn=FALSE), collapse=""))

total_removed_friends <- length(removed_friends$deleted_friends)

sent_friend_requests <- fromJSON(paste(readLines(paste0(Data_directory,'\\friends\\sent_friend_requests.json'),warn=FALSE), collapse=""))

total_sent_friend_requests <- length(sent_friend_requests$sent_requests)

your_group_membership_activity <- fromJSON(paste(readLines(paste0(Data_directory,'\\groups\\your_group_membership_activity.json'),warn=FALSE), collapse=""))

total_groups <- length(your_group_membership_activity$groups_joined)


pages <- fromJSON(paste(readLines(paste0(Data_directory,'\\likes_and_reactions\\pages.json'),warn=FALSE), collapse=""))

total_pages_liked <- length(pages$page_likes)


posts_and_comments <- fromJSON(paste(readLines(paste0(Data_directory,'\\likes_and_reactions\\posts_and_comments.json'),warn=FALSE), collapse=""))

total_posts_and_comments <- length(posts_and_comments$reactions)



###
total_user <- 0
total <- 0
for(i in 1:total_posts_and_comments){
  if(posts_and_comments$reactions[[1]]$data[[1]]$reaction$reaction == 'LIKE'){
    total <- total + 1 
  }
  if(length(grep(User_Name,posts_and_comments$reactions[[1]]$data[[1]]$reaction$actor))!=0){
    total_user <- 1 + total_user
  }
}
total_likes <- total

location_history <- fromJSON(paste(readLines(paste0(Data_directory,'\\location\\location_history.json'),warn=FALSE), collapse=""))

total_location_history <- length(location_history$location_history)

list_locations <- c()
for(i in 1:total_location_history){
  list_locations <- c(list_locations,location_history$location_history[[i]]$name)
}

table_locations <- table(list_locations)

unique_list_locations <- unique(list_locations)


data_frame <- cbind.data.frame(names(table_locations),as.integer(table_locations))



#paste0(Data_directory,'/messages/stickers_used/')

total_stickers_used_in_messages <- length(list.files(paste0(Data_directory,'/messages/stickers_used/'), pattern=NULL, full.names = TRUE, recursive = TRUE))

message_folders <- list.dirs(paste0(Data_directory,'/messages/inbox/'), full.names = TRUE, recursive = TRUE)

total_message_folders <- length(message_folders)

not_connected <- c()
groups_chats_not_connected <- 0
connected_messages <- c()
connected_messages_length <- c()

for(folder in 2:length(message_folders)){
  
if(file.exists(paste0(message_folders[folder],'/message.json'))){  
  message <- fromJSON(paste(readLines(paste0(message_folders[folder],'/message.json'),warn=FALSE), collapse=""))  
  
  if(length(message$messages) == 1){
    for(i in 1:length(message$participants)){
      not_connected_messages <- c(not_connected,message$participants[[i]]$name)  
    }
    if(length(message$participants) > 2){
      groups_chats_not_connected <-  groups_chats_not_connected + 1 
    }
  }
  
  if((length(message$messages) > 1) & (length(message$participants) == 2)){
      connected_messages <- c(connected_messages,message$participants[[1]]$name)
      connected_messages_length <- c(connected_messages_length,length(message$messages))
  }
  }
}

photos_and_videos_folders <- list.dirs(paste0(Data_directory,'/photos_and_videos'), full.names = TRUE, recursive = TRUE)


album <- list.dirs(paste0(photos_and_videos_folders,"\\album"))

files <- list.files(album)
for(f in files){
  if(endsWith(f,".json")){
    file_read <- fromJSON(paste(readLines(paste0(album,"\\",f),warn=FALSE), collapse=""))
    if(!(exists("album_info"))){
      album_info <- data.frame(album=file_read$name,number=length(file_read$photos))
    }
    else{
      album_info <- rbind(album_info,data.frame(album=file_read$name,number=length(file_read$photos)))
   
    }
  }
}


wallposts <- fromJSON(paste(readLines(paste0(Data_directory,"\\posts\\other_people's_posts_to_your_timeline.json"),warn=FALSE), collapse=""))
total_wallposts <- length(wallposts$wall_posts_sent_to_you)

statusupdates <- fromJSON(paste(readLines(paste0(Data_directory,"\\posts\\your_posts.json"),warn=FALSE), collapse=""))

total_statusupdates <- length(statusupdates$status_updates)

print("HI")

posts_info <- data.frame(type="Posted by others",number=total_wallposts)

posts_info <- rbind(posts_info,data.frame(type="Posted by me",number=total_statusupdates))

