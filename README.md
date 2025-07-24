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
See blog [How to get RFC call traces to build authorizations for S_RFC for free!](https://community.sap.com/t5/application-development-blog-posts/how-to-get-rfc-call-traces-to-build-authorizations-for-s-rfc-for-free/ba-p/12900590)  
Standard transaction `STRFCTRACE` can replace this Z-report partially (see note [2080378](https://me.sap.com/notes/2080378))  
Updated 18.01.2023 Tooltip for column Logon Procedure (trusted, basic, no user)  
Updated 14.09.2023 Show SNC status of outgoing destinations  
Updated 15.09.2023 Show http connections, too  
Updated 16.06.2025 Option to show entries with different local and remote users only  

- Report [`ZSM04000_SNC`](/ABAP/basis/zsm04000_snc.prog.abap)  
Show SNC status of active users on current application server  
See blog [Report ZSM04000_SNC – Show SNC status of current user sessions](https://community.sap.com/t5/application-development-blog-posts/report-zsm04000-snc-zrsusr000-620-show-snc-status-of-current-user-sessions/ba-p/13027982)  
You can use the Z-reports from note [748424](https://me.sap.com/notes/748424) - Evaluation of SAP GUI versions and patches  
Updated 09.05.2023 Tooltip for column Logon Procedure (trusted, basic, no user)  

- Report [`ZCLEANUP_PASSWORD_HASH_VALUESX`](/ABAP/basis/zcleanup_password_hash_valuesx.prog.abap)  
Remove all weak password hash values in user master data, change documents and password history  
See blog [Remove weak password hash values](https://community.sap.com/t5/application-development-blog-posts/report-zcleanup-password-hash-valuesx-remove-weak-password-hash-values/ba-p/13525553)  
Updated 22.12.2022  

- Report [`ZSHOW_SECPOL`](/ABAP/basis/zshow_secpol.prog.abap)  
Show security policy attributes (SECPOL) and compare them with the default values  
See blog [Show overview about security policies (SECPOL)](https://community.sap.com/t5/application-development-blog-posts/show-overview-about-security-policies-secpol/ba-p/13535999)  
Updated 19.10.2022 Selection mode: single cell  
Updated 05.02.2024 Extension to 40 columns  
Updated 29.07.2024 Replace CALL 'C_SAPGPARAM' with CL_SPFL_PROFILE_PARAMETER (note [3334028](https://me.sap.com/notes/3334028))  

- Report [`ZSECPOL_API`](/ABAP/basis/zsecpol_api.prog.abap)  
Example for using the API to manage security policies (SECPOL)  
Created 01.07.2024 New  

- Report [`ZSPFRECOMMENDED`](/ABAP/basis/zspfrecommended.prog.abap)  
Show recommended profile parameter values according to the secure-by-default project of S/4HANA  
See corresponding chapter at [SAP Secure By Default for S/4HANA on-premise 2022](https://help.sap.com/docs/SUPPORT_CONTENT/security/3362974695.html)  
Updated 19.04.2023 Show long lines in a textedit control; Change recommendation for rdisp/gui_auto_logout from 1H to 3600  
Updated 15.11.2023 Parameter added for S/4HANA 2023  
Updated 17.01.2025 Parameter added for new entries in ECS note [3250501](https://me.sap.com/notes/3250501) version 27 from 17.01.2025  
Updated 20.02.2025 Use different colors; Exception for rdisp/TRACE_HIDE_SEC_DATA  
Updated 14.05.2025 Parameter added for S/4HANA 2023; Compare recommended value with actual unsubstituted value  
Updated 03.07.2025 Changed value for parameter ssl/ciphersuites in S/4HANA 2025  

- Report [`ZSHOW_GWMON_LOG`](/ABAP/basis/zshow_gwmon_log.prog.abap)  
Show settings, and log and trace files of the RFC gateway  
Updated 31.01.2023  

- Report [`ZRSPFPAR_DYNAMIC_CD`](/ABAP/basis/zrspfpar_dynamic_cd.prog.abap)  
Show history of dynamic profile parameters  
Updated 29.03.2023 Show all instance specific change documents (and the changing client if available depending on the release)  

- Report [`ZSUSR_SNC_GUIFLAG`](/ABAP/basis/zsusr_snc_guiflag.prog.abap)  
Set/unset the SNC GUIFLAG of users which permits/disallows password based logon instead of using single sign-on  
New 14.09.2023  
Updated 15.09.2023 Refactored for using an interactive ALV  

- Report [`ZRSAU_API_GET_LOG_DATA`](/ABAP/basis/zrsau_api_get_log_data.prog.abap)  
Show usage of RFC function RSAU_API_GET_LOG_DATA to get event from the Security Audit Log  
The report requires SAP_BASIS 7.50 as well as note [3054326](https://me.sap.com/notes/3054326) - API for remote reading of audit logs as of 7.50  
12.03.2024 Initial version  
08.07.2024 Improved robustness for older releases or support packages  

- Report [`ZBNAME_RESTRICT`](/ABAP/basis/zbname_restrict.prog.abap)  
Check user names concerning parameter `BNAME_RESTRICT` in table `PRGN_CUST`  
see [`Note 1731549`](https://me.sap.com/notes/1731549)  
or [`Online Help`](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c6e6d078ab99452db94ed7b3b7bbcccf/8a922c9d7bca45c9b29bff3c59b344df.html)  

- Report [`ZSHOW_UCON_RFC_DATA`](/ABAP/basis/zshow_ucon_rfc_data.prog.prog.abap)  
Maintain UCON settings for RFC functions similar like transaction `UCONCOCKPIT`.  
The report shows additional selections and information about:  
Function groups  
Packages  
Software components  
Switched components  
Blocklist for S/4HANA   
Authorizations of the called user  
New 26.04.2024  
Updated 14.06.2024  

- Report [`ZSICFCHK`](/ABAP/basis/zsicfchk.prog.abap)  
Show public ICF services and services with logon data  
This is an extended version of standard report RSICFCHK  
23.09.2021 Initial version  

- Report [`ZUSER_ADDR_CD`](/ABAP/basis/zuser_addr_cd.prog.abap)  
Show change documents for address data of users  
These change documents are not yet integrated into transaction SUIM report RSUSR100N.  
Change documents with empty old and new value are omitted.  
Limitations:  
The change documents for the corresponding business partner are slightly different.  
Change documents of deleted users are not shown.  
Instead of reading table USR21, the report may should get the change documents for address assignments.  
20.01.2025 Initial version  

- Report [`ZTOUCH_PROJVIEW_DEP_PROGS`](/ABAP/basis/ztouch_projview_dep_progs.prog.abap)  
This is an extended version of the report provided by notes 3565944, 3577258 about a Memory Corruption vulnerability  
16.04.2025 Initial version  

- Report [`ZSHOW_DUMP_RFC_NO_AUTHORITY`](/ABAP/basis/zshow_dump_rfc_no_authority.prog.abap)  
Show dumps for runtime error RFC_NO_AUTHORITY  
This report is useful for troubleshooting missing authorizations for S_RFC, especially concerting SAP security note [3600840](https://me.sap.com/notes/notes/3600840)  
08.07.2025 Initial version  

- Report [`ZSAP4ME_NOTE_SEARCH`](/ABAP/basis/zsap4me_note_search.prog.abap)  
Construct an URL to search for notes in the SAP Support Portal.  
The expert search for notes in the SAP Support Portal uses a new URL pattern since July 2025. This report constructs such URLs. Most selection fields are supported, including for example "Support Packages, greater than". Using this selection you can find ABAP correction notes, security notes, performance notes, etc. which most likely can be implemented in the system using the note assistant, transaction SNOTE.  
The value help for Software Components and Support Packages show the currently installed software units.  
Before calling the default browser, you get a popup showing the constructed URL.  
20.07.2025 Initial version  

## SAP Solution Manager (SolMan)

### Security Optimization Service

- Report [`ZSOS_OVERVIEW`](/ABAP/solman/zsos_overview.prog.abap)  
Show overview about results from the Security Optimization Service  
See blog [Show the results of the Security Optimization Service](https://community.sap.com/t5/technology-blogs-by-sap/show-the-results-of-the-security-optimization-service/ba-p/13532666)  
Updated 30.05.2023 Show user count for SOS, too  
Updated 04.09.2023 Process not only GSS SOS but normal SOS as well  

### System Recommendations

- Report [`ZSYSREC_NOTELIST_72_SP08`](/ABAP/solman/zsysrec_notelist_72_sp08.prog.abap)  
Show results from application System Recommendations  
See blog [Report ZSYSREC_NOTELIST – Show results of System Recommendation](https://community.sap.com/t5/application-development-blog-posts/report-zsysrec-notelist-show-results-of-system-recommendation/ba-p/13006390)  
Updated 18.04.2023 Solved error which was introduced in recent update from February 2023  
Updated 28.03.2024 Opion to restrict the size of the header (important for backgroud processing)  

- Report [`ZCHECK_NOTE_2934135`](/ABAP/solman/zcheck_note_2934135.prog.abap)  
Check the implementation status of note [2934135](https://me.sap.com/notes/2934135) for connected Java systems  
See note [2953257](https://me.sap.com/notes/notes/2953257)  
Updated 28.08.2020  

- Report [`ZCHECK_NOTE_3089413`](/ABAP/solman/zcheck_note_3089413.prog.abap)  
Check the implementation status of note [3089413](https://me.sap.com/notes/3089413) for connected ABAP systems  
See Security Notes Webinar [2023-02](https://help.sap.com/docs/SUPPORT_CONTENT/security/3362974335.html)  
Updated 28.03.2023 New check about generic authorizations for S_RFCACL (configuration in CCDB needed)  
Updated 29.06.2023 Updated Kernel prerequisites as described in note 3224161  
Updated 29.06.2023 Updated Note prerequisites for note 3287611 v9  
Updated 06.07.2023 Typo in text corrected  
Updated 08.09.2023 Extended syntax check  

### Configuration Validation

- Report [`ZSHOW_CCDB_CUSTOMIZING`](/ABAP/solman/zshow_ccdb_customizing.prog.abap)  
Show Store Customization of CCDB  
Updated 19.04.2023 Corrections for showing only systems which use a specific customizing  
Updated 08.09.2023 Extended syntax check  

- Report [`ZDIAGCV_TSCUS_HDR`](/ABAP/solman/zdiagcv_tscus_hdr.prog.abap)  
Maintain descriptions of Target Systems of application Configuration Validation  
See ZIP archive [Security Baseline Template](https://support.sap.com/content/dam/support/en_us/library/ssp/offerings-and-programs/support-services/sap-security-optimization-services-portfolio/Security_Baseline_Template_V2.zip)  
Updated 02.09.2022  
Updated 25.03.2024 Selection by description added  

- Report [`ZDSH_BUILDER_SHOW`](/ABAP/solman/zdsh_builder_show.prog.abap)  
Show Dashboard Builder definitions  
See see ZIP archive [Security Baseline Template](https://support.sap.com/content/dam/support/en_us/library/ssp/offerings-and-programs/support-services/sap-security-optimization-services-portfolio/Security_Baseline_Template_V2.zip)  
Updated 29.07.2022  

- Report [`ZDIAGST_GET_STORES`](/ABAP/solman/zdiagst_get_stores.prog.abap)  
Show Configuration Stores  

- Report [`ZSHOW_KERNEL_STORES`](/ABAP/solman/zshow_kernel_stores.prog.abap)  
Show ABAP release, Kernel patch level and version of the CommonCryptoLib using the configuration stores SAP_KERNEL and CRYPTOLIB  

## SAP Focused Run (FRUN)

### Configuration & Security Analysis

- Report [`ZCCDB_GET_STORES`](/ABAP/frun/zccdb_get_stores.prog.abap)  
Show configuration stores and content  
New 27.01.2023  
Updated 21.05.2024 Enhance robustness if case of no data  

- Report [`ZSHOW_TARGET_SYSTEM`](/ABAP/frun/zshow_target_system.prog.abap)  
Show CSA target systems (policies)  
Updated 27.04.2023 Show button to call CSA policy management  

- Report [`ZSHOW_COMPOSITE_POLICIES`](/ABAP/frun/zshow_composite_policies.prog.abap)  
Show CSA Composite policies  
New 27.04.2023  
Updated 22.02.2025 Value help added  

- Report [`ZCHECK_NOTE_3089413_FRUN`](/ABAP/frun/zcheck_note_3089413_frun.prog.abap)  
Check the implementation status of note [3089413](https://me.sap.com/notes/3089413) for connected ABAP systems.  
You find a corresponding FRUN policy on page [3089413.xml](/FRUN_Policies/Note_3089413.xml).  
See Security Notes Webinar [2023-02](https://help.sap.com/docs/SUPPORT_CONTENT/security/3362974335.html)  
Updated 13.03.2023 Updated note 3287611, new note 3304520  
Updated 10.07.2023 Updated Kernel prerequisites as described in note 3224161  
Updated 10.07.2023 Updated Note prerequisites for note 3287611 v9   

- FRUN Policies for specific Security Notes  
Note [3089413](https://me.sap.com/notes/3089413) - [CVE-2023-0014] Capture-replay vulnerability in SAP NetWeaver AS for ABAP and ABAP Platform: [Note_3089413.xml](/FRUN_Policies/Note_3089413.xml)  
Note [3594142](https://me.sap.com/notes/3594142) - [CVE-2025-31324] Missing Authorization check in SAP NetWeaver (Visual Composer development server): [Note_33594142.xml](/FRUN_Policies/Note_3594142.xml)  
Note [3604119](https://me.sap.com/notes/3604119) - [CVE-2025-42999] Insecure Deserialization in SAP NetWeaver (Visual Composer development server): [Note_33604119.xml](/FRUN_Policies/Note_3604119.xml)  
Instead of uploading the individual policy files for note [3594142](https://me.sap.com/notes/3594142) and [3604119](https://me.sap.com/notes/3604119) you can use following composite policy container: [CsaCont-SecNote_VCFRAMEWORK-[0000]](/FRUN_Policies/CsaCont-SecNote_VCFRAMEWORK-[0000].xml)  

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
Copyright (c) 2023 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.
