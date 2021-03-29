# ABAP Code Review Guide
ABAP Code Review Guide is a collection of ideas, tools, and approaches for implementing ABAP code reviews in various landscapes.

The document is provided as open source under the [CC license](LICENSE), suggestions and bugfixes are welcome.

Latest release from main can be found out [abap-code-review-guide.pdf](https://github.com/SAP/styleguides/releases/download/latest/abap-code-review-guide.pdf).

## Building Locally
The PDF file can be built locally, if [Node.js](https://nodejs.org/en/) is installed, by running

`npm install && npm test`

## Editing Locally
The document is written in [AsciiDoc](https://asciidoc.org), and can be edited in any text editor.

[vscode](https://code.visualstudio.com) with the [AsciiDoc extension](https://marketplace.visualstudio.com/items?itemName=asciidoctor.asciidoctor-vscode) provides preview directly in the editor.

## Automatic Build
Each time a commit is pushed(except from forks), GitHub actions will run, build the PDF and attach it to the actions run as an artifact.

When changes is pushed to the default branch, the [latest release](https://github.com/SAP/styleguides/releases/download/latest/abap-code-review-guide.pdf) will be updated with the latest version of the document.

# Thanks To
* [Christoph Pohl](https://github.com/xtough/), [@sap](https://github.com/sap)
* [Lars Hvam Petersen](https://github.com/larshp), [@heliconialabs](https://github.com/heliconialabs)
