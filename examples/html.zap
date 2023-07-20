css`
    body {
        background-color: #000;
        color: #fff;
    }
`

name: str = "joe"

html`
    <html>
        <head>
            <title>Test</title>
        </head>
        <body>
            <h1>Hi ${name}</h1>
        </body>
    </html>
`

sql`
    SELECT * FROM users
    WHERE id = 1
`