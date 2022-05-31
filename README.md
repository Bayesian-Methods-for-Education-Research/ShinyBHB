# BDB
## Introduction
Some major files:

```ui.R``` clarifies the basic layout of the app, including the tabs and items inside every page.

```server.R``` includes the algorithms of BDB and other borrowing methods, and the process of taking inputs and running data.

```global.R``` defines global variables and packages.

```data.R```, ```model.R```, ```diagnostics.R```, ```result.R``` respectively builds  ```Data```, ```Estimation```, ```Diagnostics```, ```Result``` tabs in the front-end system. In detail,<br />
```data.R``` handles input and specifies the general type of data;<br />
```model.R``` askes users to customize the model;<br />
```diagnostics.R``` gives basic metrics and show diagnostic plots;<br />
```Result``` presents an overview of the result.

## Configuration
Download sample data [here](www.google.com)