package Net::IMAP::Client;

use vars qw[$VERSION];
$VERSION = '0.2';

use strict;
use warnings;

use List::Util qw( min max first );
use IO::Socket::INET ();
use IO::Socket::SSL ();

use Net::IMAP::Client::MsgSummary ();

our $SYSREAD_BUFFER = 4096;
my %UID_COMMANDS = map { $_ => 1 } qw( COPY FETCH STORE SEARCH SORT THREAD );
my %DEFAULT_ARGS = (
    uid_mode => 1,
    timeout  => 90,
    server   => '127.0.0.1',
    port     => undef,
    user     => undef,
    pass     => undef,
    ssl      => 0,
    socket   => undef,
    _cmd_id  => 0,
);

sub new {
    my ($class, %args) = @_;

    my $self = { map {
        $_ => exists $args{$_} ? $args{$_} : $DEFAULT_ARGS{$_}
    } keys %DEFAULT_ARGS };

    bless $self, $class;

    $self->{notifications} = [];
    $self->{greeting} = $self->_socket_getline;
    return $self;
}

sub DESTROY {
    my ($self) = @_;
    $self->quit
      if $self->_get_socket->opened;
}

sub uid_mode {
    my ($self, $val) = @_;
    if (defined($val)) {
        return $self->{uid_mode} = $val;
    } else {
        return $self->{uid_mode};
    }
}

### imap utilities ###

sub login {
    my ($self, $user, $pass) = @_;
    $user ||= $self->{user};
    $pass ||= $self->{pass};
    $self->{user} = $user;
    $self->{pass} = $pass;
    _string_quote($user);
    _string_quote($pass);
    my ($ok) = $self->_tell_imap(LOGIN => "$user $pass");
    return $ok;
}

sub logout {
    my ($self) = @_;
    $self->_send_cmd('EXPUNGE');
    $self->_send_cmd('LOGOUT');
    $self->_get_socket->close;
    return 1;
}

*quit = \&logout;

sub capability {
    my ($self, $requirement) = @_;
    my $capability = $self->{capability};
    unless ($capability) {
        my ($ok, $lines) = $self->_tell_imap('CAPABILITY');
        if ($ok) {
            my $line = $lines->[0][0];
            if ($line =~ /^\*\s+CAPABILITY\s+(.*?)\s*$/) {
                $capability = $self->{capability} = [ split(/\s+/, $1) ];
            }
        }
    }
    if ($requirement && $capability) {
        return first { $_ =~ $requirement } @$capability;
    }
    return $capability;
}

sub status {
    my $self = shift;
    my $a;
    my $wants_one = undef;
    if (ref($_[0]) eq 'ARRAY') {
        my @tmp = @{$_[0]};
        $a = \@tmp;
    } else {
        $a = [ shift ];
        $wants_one = 1;
    }
    foreach (@$a) {
        _string_quote($_);
        $_ = "STATUS $_ (MESSAGES RECENT UNSEEN UIDNEXT UIDVALIDITY)";
    }
    my $results = $self->_tell_imap2(@$a);
    my %ret;
    my $name;
    foreach my $i (@$results) {
        if ($i->[0]) {          # was successful?
            my $tokens = _parse_tokens($i->[1]->[0]);
            $name = $tokens->[2];
            $tokens = $tokens->[3];
            my %tmp = @$tokens;
            $tmp{name} = $name;
            $ret{$name} = \%tmp;
        }
    }
    return $wants_one ? $ret{$name} : \%ret;
}

sub select {
    my ($self, $folder) = @_;
    my $quoted = $folder;
    _string_quote($quoted);
    my ($ok, $lines) = $self->_tell_imap(SELECT => $quoted);
    if ($ok) {
        $self->{selected_folder} = $folder;
        my %info = ();
        foreach my $tmp (@$lines) {
            my $line = $tmp->[0];
            if ($line =~ /^\*\s+(\d+)\s+EXISTS/i) {
                $info{messages} = $1 + 0;
            } elsif ($line =~ /^\*\s+FLAGS\s+\((.*?)\)/i) {
                $info{flags} = [ split(/\s+/, $1) ];
            } elsif ($line =~ /^\*\s+(\d+)\s+RECENT/i) {
                $info{recent} = $1 + 0;
            } elsif ($line =~ /^\*\s+OK\s+\[(.*?)\s+(.*?)\]/i) {
                my ($flag, $value) = ($1, $2);
                if ($value =~ /\((.*?)\)/) {
                    $info{sflags}->{$flag} = [split(/\s+/, $1)];
                } else {
                    $info{sflags}->{$flag} = $value;
                }
            }
        }
        $self->{FOLDERS} ||= {};
        $self->{FOLDERS}{$folder} = \%info;
    }
    return $ok;
}

sub separator {
    my ($self) = @_;
    my $sep = $self->{separator};
    if (!$sep) {
        my ($ok, $lines) = $self->_tell_imap(LIST => '"" ""');
        if ($ok) {
            my $tokens = _parse_tokens($lines->[0]);
            $sep = $self->{separator} = $tokens->[3];
        } else {
            $sep = undef;
        }
    }
    return $sep;
}

sub folders {
    my ($self) = @_;
    my ($ok, $lines) = $self->_tell_imap(LIST => '"" "*"');
    if ($ok) {
        my @ret = map { _parse_tokens($_)->[4] } @$lines;
        return wantarray ? @ret : \@ret;
    }
    return undef;
}

sub folders_more {
    my ($self) = @_;
    my ($ok, $lines) = $self->_tell_imap(LIST => '"" "*"');
    if ($ok) {
        my %ret = map {
            my $tokens = _parse_tokens($_);
            my $flags = $tokens->[2];
            my $sep   = $tokens->[3];
            my $name  = $tokens->[4];
            ( $name, { flags => $flags, sep => $sep } );
        } @$lines;
        return \%ret;
    }
    return undef;
}

sub noop {
    my ($self) = @_;
    my ($ok) = $self->_tell_imap('NOOP', undef, 1);
    return $ok;
}

sub seq_to_uid {
    my ($self, @seq_ids) = @_;
    my $ids = join(',', @seq_ids);

    my $save_uid_mode = $self->uid_mode;
    $self->uid_mode(0);
    my ($ok, $lines) = $self->_tell_imap(FETCH => "$ids UID", 1);
    $self->uid_mode($save_uid_mode);

    if ($ok) {
        my %ret = map {
            $_->[0] =~ /^\*\s+(\d+)\s+FETCH\s*\(\s*UID\s+(\d+)/
              && ( $1, $2 );
        } @$lines;
        return \%ret;
    }
    return undef;
}

sub search {
    my ($self, $criteria, $sort, $charset) = @_;

    $charset ||= 'UTF-8';
    _string_quote($charset);

    my $cmd = $sort ? 'SORT' : 'SEARCH';
    if ($sort) {
        if (ref($sort) eq 'ARRAY') {
            $sort = uc '(' . join(' ', @$sort) . ')';
        } elsif ($sort !~ /^\(/) {
            $sort = uc "($sort)";
        }
        $sort =~ s/\s*$/ /;
        $sort =~ s/\^/REVERSE /g;
    } else {
        $charset = "CHARSET $charset";
        $sort = '';
    }

    if (ref($criteria) eq 'HASH') {
        my @a;
        while (my ($key, $val) = each %$criteria) {
            my $quoted = $val;
            _string_quote($quoted);
            push @a, uc $key, $quoted;
        }
        $criteria = '(' . join(' ', @a) . ')';
    }

    my ($ok, $lines) = $self->_tell_imap($cmd => "$sort$charset $criteria", 1);
    if ($ok) {
        # it makes no sense to employ the full token parser here
        my $line = $lines->[0]->[0];
        $line =~ s/^\*\s+(?:SEARCH|SORT)\s+//ig;
        $line =~ s/\s*$//g;
        return [ map { $_ + 0 } split(/\s+/, $line) ];
    }

    return undef;
}

sub get_rfc822_body {
    my ($self, $msg) = @_;
    my $wants_many = undef;
    if (ref($msg) eq 'ARRAY') {
        $msg = join(',', @$msg);
        $wants_many = 1;
    }
    my ($ok, $lines) = $self->_tell_imap(FETCH => "$msg RFC822", 1);
    if ($ok) {
        my @ret = map { $_->[1] } @$lines;
        return $wants_many ? \@ret : $ret[0];
    }
    return undef;
}

sub get_part_body {
    my ($self, $msg, $part) = @_;
    my ($ok, $lines) = $self->_tell_imap(FETCH => "$msg BODY[$part]", 1);
    if ($ok) {
        return $lines->[0]->[1];
    }
    return undef;
}

sub get_parts_bodies {
    my ($self, $msg, $parts) = @_;
    $parts = join(' ', map { "BODY[$_]" } @$parts);
    my ($ok, $lines) = $self->_tell_imap(FETCH => "$msg ($parts)", 1);
    if ($ok) {
        my $a = $lines->[0];
        my %ret = ();
        for (my $i = 0; $i < @$a;) {
            my ($foo, $bar) = ($a->[$i++], $a->[$i++]);
            if ($foo =~ /BODY\[(.*?)\]\s*$/) {
                $ret{$1} = $bar;
            }
        }
        return \%ret;
    }
    return undef;
}

sub get_summaries {
    my ($self, $msg) = @_;
    my @lines;
    if (!$msg) {
        $msg = '1:*';
    } elsif (ref $msg eq 'ARRAY') {
        $msg = join(',', @$msg);
    }
    my ($ok, $lp) = $self->_tell_imap(FETCH => qq[$msg (UID FLAGS INTERNALDATE RFC822.SIZE ENVELOPE BODYSTRUCTURE)], 1);
    if ($ok) {
        my @ret;
        foreach (@$lp) {
            my $summary = $self->_make_summary(_parse_tokens($_));
            push @ret, $summary
              if $summary;
        }
        return \@ret;
    } else {
        return undef;
    }
}

sub last_error {
    my ($self) = @_;
    return $self->{_error};
}

sub notifications {
    my ($self) = @_;
    my $tmp = $self->{notifications};
    $self->{notifications} = [];
    return wantarray ? @$tmp : $tmp;
}

##### internal stuff #####

sub _get_port {
    my ($self) = @_;
    return $self->{port} || ($self->{ssl} ? 993 : 143);
}

sub _get_timeout {
    my ($self) = @_;
    return $self->{timeout} || 90;
}

sub _get_server {
    my ($self) = @_;
    return $self->{server};
}

sub _get_socket {
    my ($self) = @_;
    return $self->{socket} ||= ($self->{ssl} ? 'IO::Socket::SSL' : 'IO::Socket::INET')->new(
        PeerAddr => $self->_get_server,
        PeerPort => $self->_get_port,
        Timeout  => $self->_get_timeout,
        Proto    => 'tcp',
    );
}

sub _get_next_id {
    return ++$_[0]->{_cmd_id};
}

sub _socket_getline {
    local $/ = "\r\n";
    return $_[0]->_get_socket->getline;
}

sub _send_cmd {
    my ($self, $cmd, $args) = @_;
    local $\;
    my $id   = $self->_get_next_id;
    if ($self->uid_mode && exists($UID_COMMANDS{$cmd})) {
        $cmd = "UID $cmd";
    }
    $cmd = "NIC$id $cmd" . ($args ? " $args" : '') . "\r\n";
    # print STDERR $cmd;
    $self->_get_socket->syswrite($cmd);
    return "NIC$id";
}

sub _read_literal {
    my ($self, $count) = @_;

    my $buf;
    my @lines = ();
    my $sock = $self->_get_socket;
    while ($count > 0) {
        my $read = $sock->sysread($buf, min($count, $SYSREAD_BUFFER));
        $count -= $read;
        last if !$read;
        push @lines, $buf;
    }

    $buf = join('', @lines);
    return \$buf;
}

sub _cmd_ok {
    my ($self, $res, $id) = @_;
    $id ||= $self->{_cmd_id};

    if ($res =~ /^NIC$id\s+OK/i) {
        return 1;
    } elsif ($res =~ /^NIC$id\s+(?:NO|BAD)(?:\s+(.+))?/i) {
        my $error = $1 || 'unknown error';
	$self->{_error} = $error;
        return 0;
    }
    return undef;
}

sub _cmd_ok2 {
    my ($self, $res) = @_;

    if ($res =~ /^(NIC\d+)\s+OK/i) {
        my $id = $1;
        return ($id, 1);
    } elsif ($res =~ /^(NIC\d+)\s+(?:NO|BAD)(?:\s+(.+))?/i) {
        my $id = $1;
        my $error = $2 || 'unknown error';
        return ($id, 0, $error);
    }
    return ();
}

sub _tell_imap {
    my ($self, $cmd, $args, $do_notf) = @_;

    $cmd = uc $cmd;
    $self->_send_cmd($cmd, $args);

    my $lineparts = [];
    my $ok;
    while (my $res = $self->_socket_getline) {
        # print STDERR "$res";
        if ($res =~ /^\*/) {
            push @$lineparts, []; # this is a new line interesting in itself
        }
        if ($res =~ /(.*)\{(\d+)\}\r\n/) {
            my ($line, $len) = ($1, $2 + 0);
            push @{$lineparts->[-1]},
              $line,
                $self->_read_literal($len);
        } else {
            $ok = $self->_cmd_ok($res);
            if (defined($ok)) {
                last;
            } else {
                push @{$lineparts->[-1]}, $res;
            }
        }
    }

    if ($do_notf) {
        no warnings 'uninitialized';
        for (my $i = scalar(@$lineparts); --$i >= 0;) {
            my $line = $lineparts->[$i];

            # 1. notifications don't contain literals
            last if scalar(@$line) != 1;

            my $text = $line->[0];

            # 2. FETCH notifications only contain FLAGS.  We make a
            #    promise never to FETCH flags alone intentionally.

            # 3. Other notifications will have a first token different
            #    from the running command

            if ( $text =~ /^\*\s+\d+\s+FETCH\s*\(\s*FLAGS\s*\(.*?\)\)/
                   || $text !~ /^\*\s+(?:\d+\s+)?$cmd/ ) {
                my $tokens = _parse_tokens($line);
                if ($self->_handle_notification($tokens, 1)) {
                    splice @$lineparts, $i, 1;
                }
                next;
            }

            last;
        }
    }

    return wantarray ? ($ok, $lineparts) : $ok ? $lineparts : undef;
}

# Variant of the above method that sends multiple commands.  After
# sending all commands to the server, it waits until all results are
# returned and puts them in an array, in the order the commands were
# sent.
sub _tell_imap2 {
    my ($self, @cmd) = @_;

    my @ids = ();
    foreach (@cmd) {
        push @ids, $self->_send_cmd($_);
    }

    my %results;
    for (0..$#cmd) {
        my $lineparts = [];
        while (my $res = $self->_socket_getline) {
            # print STDERR "2: $res";
            if ($res =~ /^\*/) {
                push @$lineparts, []; # this is a new line interesting in itself
            }
            if ($res =~ /(.*)\{(\d+)\}\r\n/) {
                my ($line, $len) = ($1, $2);
                push @{$lineparts->[-1]},
                  $line,
                    $self->_read_literal($len);
            } else {
                my ($cmdid, $ok, $error) = $self->_cmd_ok2($res);
                if (defined($ok)) {
                    $results{$cmdid} = [ $ok, $lineparts, $error ];
                    last;
                } else {
                    push @{$lineparts->[-1]}, $res;
                }
            }
        }
    }

    my @ret = @results{@ids};
    return \@ret;
}

sub _string_quote {
    $_[0] =~ s/\\/\\\\/g;
    $_[0] =~ s/\"/\\\"/g;
    $_[0] = "\"$_[0]\"";
}

sub _string_unquote {
    if ($_[0] =~ s/^"//g) {
        $_[0] =~ s/"$//g;
        $_[0] =~ s/\\\"/\"/g;
        $_[0] =~ s/\\\\/\\/g;
    }
}

##### parse imap response #####
#
# This is probably the simplest/dumbest way to parse the IMAP output.
# Nevertheless it seems to be very stable and fast.
#
# $input is an array ref containing IMAP output.  Normally it will
# contain only one entry -- a line of text -- but when IMAP sends
# literal data, we read it separately (see _read_literal) and store it
# as a scalar reference, therefore it can be like this:
#
#    [ '* 11 FETCH (RFC822.TEXT ', \$DATA, ')' ]
#
# so that's why the routine looks a bit more complicated.
#
# It returns an array of tokens.  Literal strings are dereferenced so
# for the above text, the output will be:
#
#    [ '*', '11', 'FETCH', [ 'RFC822.TEXT', $DATA ] ]
#
# note that lists are represented as arrays.
#
sub _parse_tokens {
    my ($input) = @_;

    my @tokens = ();
    my @stack = (\@tokens);

    while (my $text = shift @$input) {
        if (ref $text) {
            push @{$stack[-1]}, $$text;
            next;
        }
        while (1) {
            $text =~ m/\G\s+/gc;
            if ($text =~ m/\G[([]/gc) {
                my $sub = [];
                push @{$stack[-1]}, $sub;
                push @stack, $sub;
            } elsif ($text =~ m/\G[])]/gc) {
                pop @stack;
            } elsif ($text =~ m/\G\"((?:\\.|[^\"\\])*)\"/gc) {
                my $str = $1;
                # unescape
                $str =~ s/\\\"/\"/g;
                $str =~ s/\\\\/\\/g;
                push @{$stack[-1]}, $str; # found string
            } elsif ($text =~ m/\G(\d+)/gc) {
                push @{$stack[-1]}, $1 + 0; # found numeric
            } elsif ($text =~ m/\G([a-zA-Z0-9_\$\\.+\/*-]+)/gc) {
                my $atom = $1;
                if (lc $atom eq 'nil') {
                    $atom = undef;
                }
                push @{$stack[-1]}, $atom; # found atom
            } else {
                last;
            }
        }
    }

    return \@tokens;
}

sub _make_summary {
    my ($self, $tokens) = @_;
    ## in form: [ '*', ID, 'FETCH', [ tokens ]]

    if ($tokens->[2] eq 'FETCH') {
        my %hash = @{$tokens->[3]};
        if ($hash{ENVELOPE}) {
            # full fetch
            return Net::IMAP::Client::MsgSummary->new(\%hash);
        } else {
            # 'FETCH' (probably FLAGS) notification!
            $self->_handle_notification($tokens);
            return undef;
        }
    } else {
        # notification!
        $self->_handle_notification($tokens);
        return undef;
    }
}

sub _handle_notification {
    my ($self, $tokens, $reverse) = @_;

    no warnings 'uninitialized';
    my $not;

    my $sf = $self->{selected_folder};
    if ($sf) { # otherwise we shouldn't get any notifications, but whatever
        $sf = $self->{FOLDERS}{$sf};
        if ($tokens->[2] eq 'FETCH') {
            my %data = @{$tokens->[3]};
            if (my $flags = $data{FLAGS}) {
                $not = { seq   => $tokens->[1] + 0,
                         flags => $flags };
                if (first { $_ eq '\\Deleted' } @$flags) {
                    --$sf->{messages};
                    $not->{deleted} = 1;
                }
            }

        } elsif ($tokens->[2] eq 'EXISTS') {
            $sf->{messages} = $tokens->[1] + 0;
            $not = { messages => $tokens->[1] + 0 };

        } elsif ($tokens->[2] eq 'EXPUNGE') {
            --$sf->{messages};
            $not = { seq => $tokens->[1] + 0, destroyed => 1 };

        } elsif ($tokens->[2] eq 'RECENT') {
            $sf->{recent} = $tokens->[1] + 0;
            $not = { recent => $tokens->[1] + 0 };

        } elsif ($tokens->[1] eq 'FLAGS') {
            $sf->{flags} = $tokens->[2];
            $not = { flags => $tokens->[2] };

        } elsif ($tokens->[1] eq 'OK') {
            $sf->{sflags}{$tokens->[2][0]} = $tokens->[2][1];
        }
    }

    if (defined $not) {
        $not->{folder} = $self->{selected_folder};
        if ($reverse) {
            unshift @{$self->{notifications}}, $not;
        } else {
            push @{$self->{notifications}}, $not;
        }
        return 1;
    }

    return 0;
}

1;

__END__

=pod

=encoding utf8

=head1 NAME

Net::IMAP::Client - Not so simple IMAP client library

=head1 SYNOPSIS

    use Net::IMAP::Client;

    my $imap = Net::IMAP::Client->new(
        server => 'mail.you.com',
        user   => 'USERID',
        pass   => 'PASSWORD',
        ssl    => 1,          # (use SSL? default no)
        port   => 993         # (but defaults are sane)
    );

    # everything's useless if you can't login
    $imap->login or
      die('Login failed: ' . $imap->last_error);

    # let's see what this server knows (result cached on first call)
    my $capab = $imap->capability;
       # or
    my $knows_sort = $imap->capability( qr/^sort/i );

    # get list of folders
    my @folders = $imap->folders;

    # get total # of messages, # of unseen messages etc. (fast!)
    my $status = $imap->status(@folders); # hash ref!

    # select folder
    $imap->select('INBOX');

    # get folder hierarchy separator (cached at first call)
    my $sep = $imap->separator;

    # fetch all message ids
    my @messages = $imap->search('ALL');

    # fetch all ID-s sorted by subject
    my $messages = $imap->search('ALL', 'SUBJECT');
       # or
    my @messages = $imap->search('ALL', [ 'SUBJECT' ]);

    # fetch ID-s that match criteria, sorted by subject and reverse date
    my $messages = $imap->search({
        FROM    => 'foo',
        SUBJECT => 'bar',
    }, [ 'SUBJECT', '^DATE' ]);

    # fetch message summaries (actually, a lot more)
    my @summaries = $imap->get_summaries([ @msg_ids ]);

    foreach (@summaries) {
        print $_->uid, $_->subject, $_->date, $_->rfc822_size;
        print join(', ', @{$_->from}); # etc.
    }

    # fetch full message
    my $data = $imap->get_rfc822_body($msg_id);
    print $$data; # it's reference to a scalar

    # fetch full messageS
    my @msgs = $imap->get_rfc822_body([ @msg_ids ]);
    print $$_ for (@msgs);

    # fetch single attachment (message part)
    my $data = $imap->get_part_body($msg_id, '1.2');

=head1 DESCRIPTION

Net::IMAP::Client provides methods to access an IMAP server.  It aims
to provide a simple and clean API, while employing a rigorous parser
for IMAP responses in order to create Perl data structures from them.
The code is simple, clean and extensible.

It started as an effort to improve L<Net::IMAP::Simple> but then I
realized that I needed to change a lot of code and API so I started it
as a fresh module.  Still, the design is influenced by
Net::IMAP::Simple and I even stole a few lines of code from it ;-)
(very few, honestly).

=head1 API REFERENCE

Unless otherwise specified, if a method fails it returns I<undef> and
you can inspect the error by calling $imap->last_error.  For a
successful call most methods will return a meaningful value but
definitely not I<undef>.

=head2 new( %args )  # constructor

    my $imap = Net::IMAP::Client->new(%args);

Pass to the constructor a hash of arguments that can contain:

=over

=item - B<server> (STRING)

Host name or IP of the IMAP server.

=item - B<user> (STRING)

User ID (I<only "clear" login is supported for now!>)

=item - B<pass> (STRING)

Password

=item - B<ssl> (BOOL, optional, default FALSE)

Pass a true value if you want to use IO::Socket::SSL

=item - B<uid_mode> (BOOL, optional, default TRUE)

Wether to use UID command (see RFC2060).  Recommended.

=item - B<socket> (IO::Handle, optional)

If you already have a socket connected to the IMAP server, you can
pass it here.

=back

The constructor doesn't login to the IMAP server -- you need to call
$imap->login for that.

=head2 last_error

Returns the last error from the IMAP server.

=head2 login( $user, $pass )

Login to the IMAP server.  You can pass $user and $pass here if you
wish; if not passed, the values used in constructor will be used.

Returns I<undef> if login failed.

=head2 logout / quit

Send EXPUNGE and LOGOUT then close connection.  C<quit> is an alias
for C<logout>.

=head2 noop

"Do nothing" method that calls the IMAP "NOOP" command.  It returns a
true value upon success, L<undef> otherwise.

This method fetches any notifications that the server might have for
us and you can get them by calling $imap->notifications.  See the
L<notifications()> method.

=head2 capability(), capability( qr/^SOMETHING/ )

With no arguments, returns an array of all capabilities advertised by
the server.  If you're interested in a certain capability you can pass
a RegExp.  E.g. to check if this server knows 'SORT', you can do this:

    if ($imap->capability(/^sort$/i)) {
        # speaks it
    }

This data is cached, the server will be only hit once.

=head2 select( $folder )

Selects the current IMAP folder.  On success this method also records
some information about the selected folder in a hash stored in
$self->{FOLDERS}{$folder}.  You might want to use Data::Dumper to find
out exactly what, but at the time of this writing this is:

=over

=item - B<messages>

Total number of messages in this folder

=item - B<flags>

Flags available for this folder (as array ref)

=item - B<recent>

Total number of recent messages in this folder

=item - B<sflags>

Various other flags here, such as PERMANENTFLAGS of UIDVALIDITY.  You
might want to take a look at RFC2060 at this point. :-p

=back

This method is basically stolen from Net::IMAP::Simple.

=head2 status( $folder ), status( \@folders )

Returns the status of the given folder(s).

If passed an array ref, the return value is a hash ref mapping folder
name to folder status (which are hash references in turn).  If passed
a single folder name, it returns the status of that folder only.

    my $inbox = $imap->status('INBOX');
    print $inbox->{UNSEEN}, $inbox->{MESSAGES};
    print Data::Dumper::Dumper($inbox);

    my $all = $imap->status($imap->folders);
    while (my ($name, $status) = each %$all) {
        print "$name : $status->{MESSAGES}/$status->{UNSEEN}\n";
    }

This method is designed to be very fast when passed multiple folders.
It's I<a lot> faster to call:

    $imap->status(\@folders);

than:

    $imap->status($_) foreach (@folders);

because it sends all the STATUS requests to the IMAP server before it
starts receiving the answers.  In my tests with my remote IMAP server,
for 40 folders this method takes 0.6 seconds, compared to 6+ seconds
when called individually for each folder alone.

=head2 separator

Returns the folder hierarchy separator.  This is provided as a result
of the following IMAP command:

    FETCH "" "*"

I don't know of any way to change this value on a server so I have to
assume it's a constant.  Therefore, this method caches the result and
it won't hit the server a second time on subsequent calls.

=head2 folders

Returns a list of all folders available on the server.  In scalar
context it returns a reference to an array, i.e.:

    my @a = $imap->folders;
    my $b = $imap->folders;
    # now @a == @$b;

=head2 folders_more

Returns an hash reference containing more information about folders.
It maps folder name to an hash ref containing the following:

  - flags -- folder flags (array ref; i.e. [ '\\HasChildren' ])
  - sep   -- one character containing folder hierarchy separator
  - name  -- folder name (same as the key -- thus redundant)

=head2 seq_to_uid( @sequence_ids )

I recomment usage of UID-s only (see L<uid_mode>) but this isn't
always possible.  Even when C<uid_mode> is on, the server will
sometimes return notifications that only contain message sequence
numbers.  To convert these to UID-s you can use this method.

On success it returns an hash reference which maps sequence numbers to
message UID-s.  Of course, on failure it returns I<undef>.

=head2 search( $criteria, $sort, $charset )

Executes the "SEARCH" or "SORT" IMAP commands (depending on wether
$sort is I<undef>) and returns the results as an array reference
containing message ID-s.

Note that if you use C<$sort> and the IMAP server doesn't have this
capability, this method will fail.  Use L<capability> to investigate.

=over

=item - B<$criteria>

Can be a string, in which case it is passed literally to the IMAP
command (which can be "SEARCH" or "SORT").

It can also be an hash reference, in which case keys => values are
collected into a string and values are properly quotes, i.e.:

   { subject => 'foo',
     from    => 'bar' }

will translate to:

   'SUBJECT "foo" FROM "bar"'

which is a valid IMAP SEARCH query.

If you want to retrieve all messages (no search criteria) then pass
'ALL' here.

=item - B<$sort>

Can be a string or an array reference.  If it's an array, it will
simply be joined with a space, so for instance passing the following
is equivalent:

    'SUBJECT DATE'
    [ 'SUBJECT', 'DATE' ]

The SORT command in IMAP allows you to prefix a sort criteria with
'REVERSE' which would mean descending sorting; this module will allow
you to prefix it with '^', so again, here are some equivalent
constructs:

    'SUBJECT REVERSE DATE'
    'SUBJECT ^DATE'
    [ 'SUBJECT', 'REVERSE', 'DATE' ]
    [ 'subject', 'reverse date' ]
    [ 'SUBJECT', '^DATE' ]

It'll also uppercase whatever you passed here.

If you omit $sort (or pass I<undef>) then this method will use the
SEARCH command.  Otherwise it uses the SORT command.

=item - B<$charset>

The IMAP SORT recommendation [2] requires a charset declaration for
SORT, but not for SEARCH.  Interesting, huh?

Our module is a bit more paranoid and it will actually add charset for
both SORT and SEARCH.  If $charset is omitted (or I<undef>) the it
will default to "UTF-8", which, supposedly, is supported by all IMAP
servers.

=back

=head2 get_rfc822_body( $msg_id )

Fetch and return the full RFC822 body of the message.  B<$msg_id> can
be a scalar but also an array of ID-s.  If it's an array, then all
bodies of those messages will be fetched and the return value will be
a list or an array reference (depending how you call it).

Note that the actual data is returned as a reference to a scalar, to
speed things up.

Examples:

    my $data = $imap->get_rfc822_body(10);
    print $$data;   # need to dereference it

    my @more = $imap->get_rfc822_body([ 11, 12, 13 ]);
    print $$_ foreach @more;

        or

    my $more = $imap->get_rfc822_body([ 11, 12, 13 ]);
    print $$_ foreach @$more;

=head2 get_part_body( $msg_id, $part_id )

Fetches and returns the body of a certain part of the message.  Part
ID-s look like '1' or '1.1' or '2.3.1' etc. (see RFC2060 [1], "FETCH
Command").

=head3 Scalar reference

Note that again, this data is returned as a reference to a scalar
rather than the scalar itself.  This decision was taken purely to save
some time passing around potentially large data from Perl subroutines.

=head3 Undecoded

One other thing to note is that the data is not undecoded.  One simple
way to decode it is use Email::MIME::Encodings, i.e.:

    use Email::MIME::Encodings;
    my $summary = $imap->get_summaries(10);
    my $part = $summary->get_subpart('1.1');
    my $body = $imap->get_part_body('1.1');
    my $cte = $part->transfer_encoding;  # Content-Transfer-Encoding
    $body = Email::MIME::Encodings::decode($cte, $$body);

    # and now you should have the undecoded (perhaps binary) data.

See get_summaries below.

=head2 get_parts_bodies( $msg_id, \@part_ids )

Similar to get_part_body, but this method is capable to retrieve more
parts at once.  It's of course faster than calling get_part_body for
each part alone.  Returns an hash reference which maps part ID to part
body (the latter is a reference to a scalar containing the actual
data).  Again, the data is not unencoded.

    my $parts = $imap->get_parts_bodies(10, [ '1.1', '1.2', '2.1' ]);
    print ${$parts->{'1.1'}};

=head2 get_summaries( $msg ), get_summaries( \@msgs )

Fetches, parses and returns "message summaries".  $msg can be an array
ref, or a single id.  The return value is an array reference (in
scalar context) or a list.  If a single message was passed, then in
scalar context it returns only that message (not an array ref).

The result contains one or more L<Net::IMAP::Client::MsgSummary>
objects.  The best way to understand the result is to actually call
this function and use Data::Dumper to see its structure.

Following is the output for a pretty complicated message, which
contains an HTML part with an embedded image and an attached message.
The attached message in turn contains an HTML part and an embedded
message.

  bless( {
    'message_id' => '<48A71D17.1000109@foobar.com>',
    'date' => 'Sat, 16 Aug 2008 21:31:51 +0300',
    'to' => [
        bless( {
            'at_domain_list' => undef,
            'name' => undef,
            'mailbox' => 'kwlookup',
            'host' => 'foobar.com'
        }, 'Net::IMAP::Client::MsgAddress' )
    ],
    'cc' => undef,
    'from' => [
        bless( {
            'at_domain_list' => undef,
            'name' => 'Mihai Bazon',
            'mailbox' => 'justme',
            'host' => 'foobar.com'
        }, 'Net::IMAP::Client::MsgAddress' )
    ],
    'flags' => [
        '\\Seen',
        'NonJunk',
        'foo_bara'
    ],
    'uid' => '11',
    'subject' => 'test with message attachment',
    'rfc822_size' => '12550',
    'in_reply_to' => undef,
    'bcc' => undef,
    'internaldate' => '16-Aug-2008 21:29:23 +0300',
    'reply_to' => [
        bless( {
            'at_domain_list' => undef,
            'name' => 'Mihai Bazon',
            'mailbox' => 'justme',
            'host' => 'foobar.com'
        }, 'Net::IMAP::Client::MsgAddress' )
    ],
    'sender' => [
        bless( {
            'at_domain_list' => undef,
            'name' => 'Mihai Bazon',
            'mailbox' => 'justme',
            'host' => 'foobar.com'
        }, 'Net::IMAP::Client::MsgAddress' )
    ],
    'parts' => [
        bless( {
            'part_id' => '1',
            'parts' => [
                bless( {
                    'parameters' => {
                        'charset' => 'UTF-8'
                    },
                    'subtype' => 'html',
                    'part_id' => '1.1',
                    'encoded_size' => '365',
                    'cid' => undef,
                    'type' => 'text',
                    'description' => undef,
                    'transfer_encoding' => '7bit'
                }, 'Net::IMAP::Client::MsgSummary' ),
                bless( {
                    'disposition' => {
                        'inline' => {
                            'filename' => 'someimage.png'
                        }
                    },
                    'language' => undef,
                    'encoded_size' => '4168',
                    'description' => undef,
                    'transfer_encoding' => 'base64',
                    'parameters' => {
                        'name' => 'someimage.png'
                    },
                    'subtype' => 'png',
                    'part_id' => '1.2',
                    'type' => 'image',
                    'cid' => '<part1.02030404.05090202@foobar.com>',
                    'md5' => undef
                }, 'Net::IMAP::Client::MsgSummary' )
            ],
            'multipart_type' => 'related'
        }, 'Net::IMAP::Client::MsgSummary' ),
        bless( {
            'message_id' => '<48A530CE.3050807@foobar.com>',
            'date' => 'Fri, 15 Aug 2008 10:31:26 +0300',
            'encoded_size' => '6283',
            'to' => [
                bless( {
                    'at_domain_list' => undef,
                    'name' => undef,
                    'mailbox' => 'kwlookup',
                    'host' => 'foobar.com'
                }, 'Net::IMAP::Client::MsgAddress' )
            ],
            'subtype' => 'rfc822',
            'cc' => undef,
            'from' => [
                bless( {
                    'at_domain_list' => undef,
                    'name' => 'Mihai Bazon',
                    'mailbox' => 'justme',
                    'host' => 'foobar.com'
                }, 'Net::IMAP::Client::MsgAddress' )
            ],
            'subject' => 'Test with images',
            'in_reply_to' => undef,
            'description' => undef,
            'transfer_encoding' => '7bit',
            'parameters' => {
                'name' => 'Attached Message'
            },
            'bcc' => undef,
            'part_id' => '2',
            'sender' => [
                bless( {
                    'at_domain_list' => undef,
                    'name' => 'Mihai Bazon',
                    'mailbox' => 'justme',
                    'host' => 'foobar.com'
                }, 'Net::IMAP::Client::MsgAddress' )
            ],
            'reply_to' => [
                bless( {
                    'at_domain_list' => undef,
                    'name' => 'Mihai Bazon',
                    'mailbox' => 'justme',
                    'host' => 'foobar.com'
                }, 'Net::IMAP::Client::MsgAddress' )
            ],
            'parts' => [
                bless( {
                    'parameters' => {
                        'charset' => 'UTF-8'
                    },
                    'subtype' => 'html',
                    'part_id' => '2.1',
                    'encoded_size' => '344',
                    'cid' => undef,
                    'type' => 'text',
                    'description' => undef,
                    'transfer_encoding' => '7bit'
                }, 'Net::IMAP::Client::MsgSummary' ),
                bless( {
                    'disposition' => {
                        'inline' => {
                            'filename' => 'logo.png'
                        }
                    },
                    'language' => undef,
                    'encoded_size' => '4578',
                    'description' => undef,
                    'transfer_encoding' => 'base64',
                    'parameters' => {
                        'name' => 'logo.png'
                    },
                    'subtype' => 'png',
                    'part_id' => '2.2',
                    'type' => 'image',
                    'cid' => '<part1.02060209.09080406@foobar.com>',
                    'md5' => undef
                }, 'Net::IMAP::Client::MsgSummary' )
            ],
            'cid' => undef,
            'type' => 'message',
            'multipart_type' => 'related'
        }, 'Net::IMAP::Client::MsgSummary' )
    ],
    'multipart_type' => 'mixed'
  }, 'Net::IMAP::Client::MsgSummary' );

As you can see, the parser retrieves all data, including from the
embedded messages.

There are many other modules you can use to fetch such information.
L<Email::Simple> and L<Email::MIME> are great.  The only problem is
that you have to have fetched already the full (RFC822) body of the
message, which is impractical over IMAP.  When you want to quickly
display a folder summary, the only practical way is to issue a FETCH
command and retrieve only those headers that you are interested in
(instead of full body).  C<get_summaries> does exactly that (issues a
FETCH (FLAGS INTERNALDATE RFC822.SIZE ENVELOPE BODYSTRUCTURE)).  It's
acceptably fast even for huge folders.

=head2 notifications()

The IMAP server may send various notifications upon execution of
commands.  They are collected in an array which is returned by this
method (returns an array ref in scalar context, or a list otherwise).
It clears the notifications queue so on second call it will return an
empty array (unless new notifications were collected in the meantime).

Each element in this array (notification) is a hash reference
containing one or more or the following:

  - seq       : the *sequence number* of the changed message
  - flags     : new flags for this message
  - deleted   : when the \Deleted flag was set for this message
  - messages  : new number of messages in this folder
  - recent    : number of recent messages in this folder
  - flags     : new flags of this folder (seq is missing)
  - destroyed : when this message was expunged
  - folder    : the name of the selected folder

C<folder> is always present.  C<seq> is present when a message was
changed some flags (in which case you have C<flags>) or was expunged
(in which case C<destroyed> is true).  When C<flags> were changed and
the B<\Deleted> flag is present, you also get C<deleted> true.

C<seq> is a message sequence number.  Pretty dumb, I think it's
preferable to work with UID-s, but that's what the IMAP server
reports.  To get UID-s call seq_to_uid.

When C<flags> is present but no C<seq>, it means that the list of
available flags for the C<folder> has changed.

You get C<messages> upon an "EXISTS" notification, which usually means
"you have new mail".  It indicates the total number of messages in the
folder, not just "new" messages.  I've yet to come up with a good way
to measure the number of new/unseen messages, other than calling
C<status($folder)>.

I rarely got C<recent> from my IMAP server in my tests; if more
clients are simultaneously logged in as the same IMAP user, only one
of them will receive "RECENT" notifications; others will have to rely
on "EXISTS" to tell when new messages have arrived.  Therefore I can
only say that "RECENT" is useless and I advise you to ignore it.

=head1 TODO

There's a bunch of missing functionality which should be quite easy to
add but I didn't get to it yet.  Such as create/delete folders.  If
you need it urgently, feel free to add it and send me a patch,
otherwise wait until I need it. :-)

=over

=item - authentication schemes other than plain text (B<help wanted>)

=item - append/delete messages

=item - create/remove mailboxes

=item - expunge folder

=item - support THREAD operation

=item - reconnect/relogin when connection lost

=item - better error handling?

=back

=head1 SEE ALSO

L<Net::IMAP::Simple>, L<Mail::IMAPClient>, L<Mail::IMAPTalk>

L<Email::Simple>, L<Email::MIME>

RFC2060 [1] is a must read if you want to do anything fancier than
what this module already supports.

=head1 REFERENCES

[1] http://ietfreport.isoc.org/rfc/rfc2060.txt

[2] http://ietfreport.isoc.org/all-ids/draft-ietf-imapext-sort-20.txt

=head1 AUTHOR

Mihai Bazon, <mihai.bazon@gmail.com>
    http://www.dynarchlib.com/
    http://www.bazon.net/mishoo/

=head1 COPYRIGHT

Copyright (c) Mihai Bazon 2008.  All rights reserved.

This module is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT
WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER
PARTIES PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
SOFTWARE IS WITH YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME
THE COST OF ALL NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE LIABLE
TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES.

=cut
