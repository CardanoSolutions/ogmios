
+++
title = "Local Tx Submission"
chapter = false
weight = 2
+++


## Overview

{{% ascii-drawing %}}
 *----------*                                                
 |   Busy   |◀══════════════════════════════╗                
 *----------*            SubmitTx           ║                
      │                                     ║                
      │                                *---------*           
      │                                |         |           
      │                                |         |           
      │          SubmitTxResponse      |   Idle  |           
      └───────────────────────────────╼|         |           
                                       |         |⇦ START    
                                       *---------*           
{{% /ascii-drawing %}}

Transaction submission is pretty simple & works by submitting an already serialized and signed transaction as one single message.

In case of success, Ogmios / the node returns an empty response. Otherwise, it returns an error with some details about what went wrong. Clients must thereby know how to construct valid transactions.

🚧 Coming Soon: JavaScript Examples with Ogmios 🚧
