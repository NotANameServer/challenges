import { Socket } from 'net';

const CRLF = '\r\n';

const [,, host, username] = process.argv;

if (host && username) {
  const client = new Socket();
  let joinedChannel;

  client.setEncoding('utf8');
  process.stdin.setEncoding('utf8');

  client.on('error', console.error);
  client.on('data', (data) => {
    if (data.startsWith('PING')) {
      client.write(`PONG${CRLF}`);
      return;
    }
    console.log(data);
  });

  // 'drlazor.be:6667'.split(':', 2) -> ['drlazor.be', '6667'].reverse() -> ...['6667', 'drlazor.be'] -> '6667', 'drlazor.be'
  client.connect(...host.split(':', 2).reverse(), () => {
    console.log(`Successfully connected to ${host}!`);
    client.write(`NICK ${username}${CRLF}USER ${username} 8 * :${username}${CRLF}`);
    
    process.stdin.on('data', (data) => {
      const args = data.trim().split(' ');
      const command = args.shift();

      switch (command) {
        case ':m':
          if (args.length < 2) {
            console.error(':m <channel> <message>');
            return;
          }
          client.write(`PRIVMSG ${args[0]} :${args.slice(1).join(' ')}${CRLF}`);
          break;

        case ':j':
          if (args.length !== 1) {
            console.error(':j <channel>');
            return;
          }
          client.write(`JOIN ${joinedChannel = args[0].trim()}${CRLF}`);
          break;

        case ':n':
          if (args.length !== 1) {
            console.error(':n <nickname>');
            return;
          }
          client.write(`NICK ${args[0]}`);
          break;

        case ':q':
          client.write(`QUIT${args[0] ? ' ' : ''}${args?.join(' ') || ''}${CRLF}`);
          console.log('Stopped.');
          client.destroy();
          process.exit(1);

        default:
          if (!joinedChannel || !data?.trim()) {
            console.error('Please use one of :m, :j, :n and :q commands');
          } else {
            client.write(`PRIVMSG ${joinedChannel} :${data}${CRLF}`);
          }
      }
    });
  });
} else {
  console.error('Please specify the host and/or the username.');
}
