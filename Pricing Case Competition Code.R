Data$price = Data$InvoiceRevenue/Data$InvoiceQuantity
summary(Data)

#Transport
library('lfe')
library('data.table')
library('tidyverse')
Data_transport$size = 0
Data_transport = Data_transport[,-21]
transport = aggregate(Data_transport$InvoiceRevenue, by = list(Data_transport$CustomerAccountName), FUN = sum)
transport = transport %>%
  rename(CustomerAccountName = Group.1,
         Size = x)
Data_transport = merge(Data_transport, transport, by = 'CustomerAccountName')
reg_1 = felm(InvoiceQuantity~RateAmount+Size+factor(CountryCode)|factor(GLPeriodQuarter), data = Data_transport)
summary(reg_1)
Data_transport$Optimal_Price = 0
Data_transport$Optimal_Price[Data_transport$CountryCode == 'US' ] = -(0.0006373*Data_transport$Size+2.901)/2*(-0.4413)
Data_transport$Optimal_Price[Data_transport$CountryCode == 'CA' ] = -(0.0006373*Data_transport$Size)/2*(-0.4413)
transport_res = Data_transport[,c(5,7,9,10,11,12,21,22)]
summary(transport_res$Size[transport_res$CountryCode == 'US'])
summary(transport_res$Size[transport_res$CountryCode == 'CA'])
transport_res$Size_account[transport_res$Size<1456 & transport_res$CountryCode == 'US' ] = 'small'
transport_res$Size_account[transport_res$Size<5662 & transport_res$CountryCode == 'CA' ] = 'small'
transport_res$Size_account[transport_res$Size>118691 & transport_res$CountryCode == 'US'] = 'large'
transport_res$Size_account[transport_res$Size>62167 & transport_res$CountryCode == 'CA'] = 'large'
transport_res$Size_account[transport_res$Size>=7797 & transport_res$Size<=118691 & transport_res$CountryCode == 'US'] = 'median'
transport_res$Size_account[transport_res$Size>=5662 & transport_res$Size<=62167 & transport_res$CountryCode == 'US'] = 'median'
t_market = aggregate(transport_res$Optimal_Price, by = c(list(transport_res$Size_account), list(transport_res$MarketName)) , FUN = mean)
t_res = t_market %>%
  rename(Account_Size = Group.1,
         market = Group.2,
         optimal_price = x)

#Storage
Data_storage<-Data[Data$BillCodeType=='STORAGE - CARTON'|Data$BillCodeType=='STORAGE - LTO'|Data$BillCodeType=='STORAGE - TAPE'|Data$BillCodeType=='STORAGE - HD'|Data$BillCodeType=='STORAGE - MINIMUM STRG CHARGE'|Data$BillCodeType=='STORAGE - PALLET'|Data$BillCodeType=='STORAGE - FLAT'|Data$BillCodeType=='STORAGE - BULK'|Data$BillCodeType=='STORAGE - STRG,LOCKED AREA'|Data$BillCodeType=='Storage',]
storage = aggregate(Data_storage$InvoiceRevenue, by = list(Data_storage$CustomerAccountName), FUN = sum)
storage = storage %>%
  rename(CustomerAccountName = Group.1,
         Size = x)
Data_storage = merge(Data_storage, storage, by = 'CustomerAccountName')
reg_2 = felm(InvoiceQuantity~RateAmount+Size+factor(CountryCode)|factor(GLPeriodQuarter), data = Data_storage)
summary(reg_2)
Data_storage$Optimal_Price = 0
Data_storage$Optimal_Price[Data_storage$CountryCode == 'US' ] = -(0.001725*Data_storage$Size+65.42)/2*(-0.2032)
Data_storage$Optimal_Price[Data_storage$CountryCode == 'CA' ] = -(0.001725*Data_storage$Size)/2*(-0.2032)
storage_res = Data_storage[,c(5,7,9,10,11,12,21,22)]
summary(storage_res$Size[storage_res$CountryCode == 'US'])
summary(storage_res$Size[storage_res$CountryCode == 'CA'])
storage_res$Size_account[storage_res$Size<7797 & storage_res$CountryCode == 'US' ] = 'small'
storage_res$Size_account[storage_res$Size<5662 & storage_res$CountryCode == 'CA' ] = 'small'
storage_res$Size_account[storage_res$Size>118691 & storage_res$CountryCode == 'US'] = 'large'
storage_res$Size_account[storage_res$Size>62167 & storage_res$CountryCode == 'CA'] = 'large'
storage_res$Size_account[storage_res$Size>=7797 & storage_res$Size<=118691 & storage_res$CountryCode == 'US'] = 'median'
storage_res$Size_account[storage_res$Size>=5662 & storage_res$Size<=62167 & storage_res$CountryCode == 'US'] = 'median'
storage_market = aggregate(storage_res$Optimal_Price, by = c(list(storage_res$Size_account), list(storage_res$MarketName)) , FUN = mean)
storage_market = storage_market %>%
  rename(Account_Size = Group.1,
         market = Group.2,
         optimal_price = x)


#Service
Data_service<-Data[Data$BillCodeType=='SERVICE - PERMANENT WITHDRAWAL'|Data$BillCodeType=='SERVICE - RECEIVING AND ENTRY'|Data$BillCodeType=='SERVICE - TRANSPORTATION HANDLING'|Data$BillCodeType=='SERVICE - RETRIEVAL'|Data$BillCodeType=='SERVICE - REFILE'|Data$BillCodeType=='SERVICE - MINIMUM SERVICE CHARGE PER ORDER'|Data$BillCodeType=='Service',]
service = aggregate(Data_service$InvoiceRevenue, by = list(Data_service$CustomerAccountName), FUN = sum)
service = service %>%
  rename(CustomerAccountName = Group.1,
         Size = x)
Data_service = merge(Data_service, service, by = 'CustomerAccountName')
reg_3 = felm(InvoiceQuantity~RateAmount+Size+factor(CountryCode)|factor(GLPeriodQuarter), data = Data_service)
summary(reg_3)
Data_service$Optimal_Price = 0
Data_service$Optimal_Price[Data_service$CountryCode == 'US' ] = -(0.0001768*Data_service$Size+0.6695)/2*(-0.003351)
Data_service$Optimal_Price[Data_service$CountryCode == 'CA' ] = -(0.0001768*Data_service$Size)/2*(-0.003351)
service_res = Data_service[,c(5,7,9,10,11,12,21,22)]
summary(service_res$Size[service_res$CountryCode == 'US'])
summary(service_res$Size[service_res$CountryCode == 'CA'])
service_res$Size_account[service_res$Size<681 & service_res$CountryCode == 'US' ] = 'small'
service_res$Size_account[service_res$Size<594.6 & service_res$CountryCode == 'CA' ] = 'small'
service_res$Size_account[service_res$Size>13928 & service_res$CountryCode == 'US'] = 'large'
service_res$Size_account[service_res$Size>9464.6 & service_res$CountryCode == 'CA'] = 'large'
service_res$Size_account[service_res$Size>=681 & service_res$Size<=13928 & service_res$CountryCode == 'US'] = 'median'
service_res$Size_account[service_res$Size>=594.6 & service_res$Size<=9464.6 & service_res$CountryCode == 'US'] = 'median'
service_market = aggregate(service_res$Optimal_Price, by = c(list(service_res$Size_account), list(service_res$MarketName)) , FUN = mean)
service_market = service_market %>%
  rename(Account_Size = Group.1,
         market = Group.2,
         optimal_price = x)


write.csv(storage_market, 'C:/Users/VE1/Downloads/storage_price.csv', row.names = FALSE)
write.csv(storage_res, 'C:/Users/VE1/Downloads/storage.csv', row.names = FALSE)
write.csv(transport_res, 'C:/Users/VE1/Downloads/transport.csv', row.names = FALSE)
write.csv(t_res, 'C:/Users/VE1/Downloads/transport_price.csv', row.names = FALSE)
write.csv(service_market, 'C:/Users/VE1/Downloads/service_price.csv', row.names = FALSE)
write.csv(service_res, 'C:/Users/VE1/Downloads/service.csv', row.names = FALSE)
