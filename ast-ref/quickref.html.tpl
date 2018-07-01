<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>cljs.analyzer AST Quickref (alpha)</title>
  <style>
  * {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
  }
  body {
    font-family: "Lucida Grande", "Trebuchet MS", "Bitstream Vera Sans", Verdana, Helvetica, sans-serif;
    font-size: 13px;
    line-height: 20px;
    margin: 40px 80px;
  }
  main {
    margin-left: 240px;
    width: 680px;
  }
  dl {
  display: flex;
  flex-flow: row wrap;

  }
  dt:nth-of-type(odd), dd:nth-of-type(odd) {
    background: #f5f5f5;
  }
  dt {
    padding: 2px 0; width: 120px;
    color:#191970;
  }
  dd { padding: 2px 0; width: 560px;}
  .wide-keys dt { width: 160px; }
  .wide-keys dd { width: 520px; }
  code, dt { font-family: "Menlo"; }
  code { color: rgb(160,40,160); }
  .page-title {
    font-size: 32px;
    border-bottom: none;
    margin-bottom: 2.75rem;
  }
  h1 {
    border-bottom: 1px solid #ccc;
    padding-bottom: 0.75em;
    margin-bottom: 1.5rem;
  }
  h2 {
    margin-bottom: 0.2rem;
    position: relative;
    padding-left: 20px;
    margin-left: -20px;
  }
  h4 {
    margin-bottom: 1rem;
  }

  h2 a {
    color: #aaa;
    font-size: 16px;
    font-weight: normal;
    margin-left: -20px;
    position: absolute;
    text-decoration: none;
    vertical-align: middle;
    visibility: hidden;
  }
  h2:hover a {
    visibility: visible;
  }
  p {
    margin-bottom: 1rem;
  }
  section {
    /*border-bottom: 1px solid #ccc;*/
    margin-bottom: 1rem;
    padding-bottom: 1rem;
  }
  nav {
    background: #f5f5f5;
    font-size: 11px;
    line-height: 14px;
    padding: 0.5rem 1rem;
    position: fixed;
    width: 160px;
  }
  nav ul {
    list-style: none;
    margin-bottom: 1rem;
  }
  nav ul:last-child { margin-bottom: 0; }
  </style>
</head>
<body>
  <nav>
    <strong>Nodes reference</strong>
    <ul>
      {nav}
    </ul>
  </nav>
  <main>
    <h1 class="page-title">cljs.analyzer AST Quickref (alpha)</h1>
    <h1>Common AST fields</h1>
    {common}
    <h1>Nodes reference</h1>
    {nodes}
  </main>
</body>
</html>
