[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/security-services-tools)](https://api.reuse.software/info/github.com/SAP-samples/security-services-tools)
# Security Services Tools

## Description

You are using security relates services and tools like the EWA, SOS, System Recommendations, Configuration Validation or a Security Dashboard in the SAP Solution Manager. You want to dig deeper into these topics and want to build own reporting capabilities on top. In this case you can use the ABAP reports in this repository as a starting point for further analysis and development.

## Basis

Report [`ZSHOW_BG_JOB_USER`](/ABAP/Basis/ZSHOW_BG_JOB_USER.abap)  
Show user type of background job steps  

Report [`ZSM04000_SNC`](/ABAP/Basis/ZSM04000_SNC.abap)  
Show SNC status of active users on current application server  
see blog [Report ZSM04000_SNC – Show SNC status of current user sessions](https://blogs.sap.com/2013/09/30/report-zsm04000snc-show-snc-status-of-current-user-sessions/)  

Report [`ZCLEANUP_PASSWORD_HASH_VALUESX`](/ABAP/Basis/ZCLEANUP_PASSWORD_HASH_VALUESX.abap)  
Remove all weak password hash values in user master data, change documents and password history  

Report [`ZSHOW_INSTALLED_COMPS](/ABAP/Basis/ZSHOW_INSTALLED_COMPS.abap)  
Show installed software components and verify the age of the support packages 

## SAP Solution Manager (SolMan)

### Security Optimization Service

Report [`ZSOS_OVERVIEW`](/ABAP/SolMan/ZSOS_OVERVIEW.abap)  
Show overview about results from the Security Optimization Service  
see blog [Show the results of the Security Optimization Service](https://blogs.sap.com/2022/01/19/show-the-results-of-the-security-optimization-service/)  

### System Recommendations

Report [`ZSYSREC_NOTELIST_72_SP08`](/ABAP/SolMan/ZSYSREC_NOTELIST_72_SP08.abap)  
Show results from application System Recommendations  
see blog [Report ZSYSREC_NOTELIST – Show results of System Recommendation](https://blogs.sap.com/2011/07/18/report-zsysrecnotelist-show-results-of-system-recommendation/)  

### Configuration Validation

Report [`ZSHOW_CCDB_CUSTOMIZING`](/ABAP/SolMan/ZSHOW_CCDB_CUSTOMIZING.abap)  
Show Store Customization of CCDB  

Report [`ZDIAGCV_TSCUS_HDR`](/ABAP/SolMan/ZDIAGCV_TSCUS_HDR.abap)  
Maintain descriptions of Target Systems of application Configuration Validation  
see ZIP archive [Security Baseline Template](https://support.sap.com/content/dam/support/en_us/library/ssp/offerings-and-programs/support-services/sap-security-optimization-services-portfolio/Security_Baseline_Template_V2.zip)

Report [`ZDSH_BUILDER_SHOW`](/ABAP/SolMan/ZDSH_BUILDER_SHOW.abap)  
Show Dashboard Builder definitions  
see see ZIP archive [Security Baseline Template](https://support.sap.com/content/dam/support/en_us/library/ssp/offerings-and-programs/support-services/sap-security-optimization-services-portfolio/Security_Baseline_Template_V2.zip)

Report [`ZDIAGST_GET_STORES`](/ABAP/SolMan/ZDIAGST_GET_STORES.abap)  
Show Configuration Stores  

Report [`ZSHOW_KERNEL_STORES`](/ABAP/SolMan/ZSHOW_KERNEL_STORES.abap)  
Show ABAP release, Kernel patch level and version of the CommonCryptoLib using the configuration stores SAP_KERNEL and CRYPTOLIB  

## Requirements
None

## Download and Installation
Use the raw view to copy & paste the source code of the reports into a custom program. (You do not need any tool like [abapGit](https://github.com/abapGit/abapGit).)

## Known Issues
No known issues.

## How to obtain support
[Create an issue](https://github.com/SAP-samples/security-services-tools/issues) in this repository if you find a bug, have a request or a suggestion about the content.  

[Start a discussion](https://github.com/SAP-samples/security-services-tools/discussions) in this repository if you have questions about the content.

[Ask the SAP security community](https://answers.sap.com/tags/49511061904067247446167091106425) in case of other topics concerning security.
<!--- https://answers.sap.com/questions/ask.html?additionalTagId=49511061904067247446167091106425 --->

## Contributing
If you wish to contribute code, offer fixes or improvements, please send a pull request. Due to legal reasons, contributors will be asked to accept a DCO when they create the first pull request to this project. This happens in an automated fashion during the submission process. SAP uses [the standard DCO text of the Linux Foundation](https://developercertificate.org/).

## License
Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.
