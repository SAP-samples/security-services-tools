[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/security-services-tools)](https://api.reuse.software/info/github.com/SAP-samples/security-services-tools)
# Security Services Tools

## Description

You are using security relates services and tools like the EWA, SOS, System Recommendations, Configuration Validation or a Security Dashboard in the SAP Solution Manager. You want to dig deeper into these topics and want to build own reporting capabilities on top. In this case you can use the ABAP reports in this repository as a starting point for further analysis and development.

## Basis

- Report [`ZSHOW_BG_JOB_USER`](/ABAP/basis/zshow_bg_job_user.prog.abap)  
Show user type of background job steps  

- Report [`ZSHOW_INSTALLED_COMPS`](/ABAP/basis/zshow_installed_comps.prog.abap)  
Show installed software components and verify the age of the support packages  

- Report [`ZRFC_STATRECS_SUMMARY`](/ABAP/basis/zrfc_statrecs_summary.prog.abap)  
Show Workload Statistic of RFC calls  
See blog [How to get RFC call traces to build authorizations for S_RFC for free!](https://blogs.sap.com/2010/12/05/how-to-get-rfc-call-traces-to-build-authorizations-for-srfc-for-free/)  
Standard transaction `STRFCTRACE` can replace this Z-report (see note [2080378](https://launchpad.support.sap.com/#/notes/2080378))  
Updated 26.08.2022  

- Report [`ZSM04000_SNC`](/ABAP/basis/zsm04000_snc.prog.abap)  
Show SNC status of active users on current application server  
See blog [Report ZSM04000_SNC – Show SNC status of current user sessions](https://blogs.sap.com/2013/09/30/report-zsm04000snc-show-snc-status-of-current-user-sessions/)  
You can use the Z-reports from note [748424](https://launchpad.support.sap.com/#/notes/748424) - Evaluation of SAP GUI versions and patches  
Updated 11.10.2022  

- Report [`ZCLEANUP_PASSWORD_HASH_VALUESX`](/ABAP/basis/zcleanup_password_hash_valuesx.prog.abap)  
Remove all weak password hash values in user master data, change documents and password history  
See blog [Remove weak password hash values](https://blogs.sap.com/2022/07/22/report-zcleanup_password_hash_valuesx-remove-weak-password-hash-values/)  
Updated 24.08.2022  

- Report [`ZSHOW_SECPOL`](/ABAP/basis/zshow_secpol.prog.abap)  
Show security policy attributes (SECPOL) and compare them with the default values  
See blog [Show overview about security policies (SECPOL)](https://blogs.sap.com/2022/10/07/show-overview-about-security-policies-secpol/)  
Updated 19.10.2022  

- Report [`ZSPFRECOMMENDED`](/ABAP/basis/zspfrecommended.prog.abap)  
Show recommended profile parameter values according to the secure-by-default project of S/4HANA  
See corresponding chapter at [SAP Secure By Default for S/4HANA on-premise 2022](https://wiki.scn.sap.com/wiki/pages/viewpage.action?pageId=635281119)  
Updated 03.11.2022  

## SAP Solution Manager (SolMan)

### Security Optimization Service

- Report [`ZSOS_OVERVIEW`](/ABAP/solman/zsos_overview.prog.abap)  
Show overview about results from the Security Optimization Service  
See blog [Show the results of the Security Optimization Service](https://blogs.sap.com/2022/01/19/show-the-results-of-the-security-optimization-service/)  
Updated 27.07.2022: You can now use the same report to view the rating overview tables of the EarlyWatch Alert (EWA).  

### System Recommendations

- Report [`ZSYSREC_NOTELIST_72_SP08`](/ABAP/solman/zsysrec_notelist_72_sp08.prog.abap)  
Show results from application System Recommendations  
See blog [Report ZSYSREC_NOTELIST – Show results of System Recommendation](https://blogs.sap.com/2011/07/18/report-zsysrecnotelist-show-results-of-system-recommendation/)  

### Configuration Validation

- Report [`ZSHOW_CCDB_CUSTOMIZING`](/ABAP/solman/zshow_ccdb_customizing.prog.abap)  
Show Store Customization of CCDB  

- Report [`ZDIAGCV_TSCUS_HDR`](/ABAP/solman/zdiagcv_tscus_hdr.prog.abap)  
Maintain descriptions of Target Systems of application Configuration Validation  
See ZIP archive [Security Baseline Template](https://support.sap.com/content/dam/support/en_us/library/ssp/offerings-and-programs/support-services/sap-security-optimization-services-portfolio/Security_Baseline_Template_V2.zip)  
Updated 02.09.2022  

- Report [`ZDSH_BUILDER_SHOW`](/ABAP/solman/zdsh_builder_show.prog.abap)  
Show Dashboard Builder definitions  
See see ZIP archive [Security Baseline Template](https://support.sap.com/content/dam/support/en_us/library/ssp/offerings-and-programs/support-services/sap-security-optimization-services-portfolio/Security_Baseline_Template_V2.zip)  
Updated 29.07.2022  

- Report [`ZDIAGST_GET_STORES`](/ABAP/solman/zdiagst_get_stores.prog.abap)  
Show Configuration Stores  

- Report [`ZSHOW_KERNEL_STORES`](/ABAP/solman/zshow_kernel_stores.prog.abap)  
Show ABAP release, Kernel patch level and version of the CommonCryptoLib using the configuration stores SAP_KERNEL and CRYPTOLIB  

## Requirements
None

## Download and Installation
Use the raw view to copy & paste the source code of the reports into a custom program.  

You can use [abapGit](https://github.com/abapGit/abapGit) to load the compleate package from branch `abapGit` into an SAP Solution Manager. In any other system you might want to use the function 'Advanced -> Selective Pull' to get only the basis objects.  

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
