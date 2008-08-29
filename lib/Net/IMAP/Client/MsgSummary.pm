package Net::IMAP::Client::MsgSummary;

use Net::IMAP::Client::MsgAddress ();

sub new {
    my ($class, $data, $part_id) = @_;
    bless my $self = {}, $class;
    if ($part_id) {
        $self->{part_id} = $part_id;
    }
    if ($data->{BODY}) {
        $self->_parse_body($data->{BODY});
    }
    if ($data->{BODYSTRUCTURE}) {
        $self->_parse_bodystructure($data->{BODYSTRUCTURE});
    }
    if ($data->{ENVELOPE}) {
        $self->_parse_envelope($data->{ENVELOPE});
    }
    if ($data->{FLAGS}) {
        $self->{flags} = $data->{FLAGS};
    }
    if ($data->{INTERNALDATE}) {
        $self->{internaldate} = $data->{INTERNALDATE};
    }
    if ($data->{'RFC822.SIZE'}) {
        $self->{rfc822_size} = $data->{'RFC822.SIZE'};
    }
    if ($data->{UID}) {
        $self->{uid} = $data->{UID};
    }
    return $self;
}

sub type { $_[0]->{type} }

sub subtype { $_[0]->{subtype} }

sub parameters { $_[0]->{parameters} }

sub cid { $_[0]->{cid} }

sub description { $_[0]->{description} }

sub transfer_encoding { $_[0]->{transfer_encoding} }

sub encoded_size { $_[0]->{encoded_size} }

sub content_type { "$_[0]->{type}/$_[0]->{subtype}" }

sub charset { $_[0]->{parameters}->{charset} }

sub filename {
    my ($self) = @_;
    my $disp = $self->{disposition};
    my $filename;
    if ($disp) {
        while (my ($key, $val) = each %$disp) {
            if (ref($val) eq 'HASH') {
                $filename = $val->{filename};
                last if $filename;
            }
        }
    }
    unless ($filename) {
        $filename = $_[0]->{parameters}->{name};
    }
    return $filename;
}

sub multipart { lc $_[0]->{multipart_type} }

sub parts { $_[0]->{parts} }

sub rfc822_size { $_[0]->{rfc822_size} }

sub internaldate { $_[0]->{internaldate} }

sub flags { $_[0]->{flags} }

sub uid { $_[0]->{uid} }

sub part_id { $_[0]->{part_id } }

sub md5 { $_[0]->{md5} }

sub disposition { $_[0]->{disposition} }

sub language { $_[0]->{language} }

# envelope

sub date { $_[0]->{date} }

sub subject { $_[0]->{subject} }

sub from { $_[0]->{from} }

sub sender { $_[0]->{sender} }

sub reply_to { $_[0]->{reply_to} }

sub to { $_[0]->{to} }

sub cc { $_[0]->{cc} }

sub bcc { $_[0]->{bcc} }

sub in_reply_to { $_[0]->{in_reply_to} }

sub message_id { $_[0]->{message_id} }

# utils

sub get_subpart {
    my ($self, $part) = @_;
    foreach my $index (split(/\./, $part)) {
        $self = $self->parts->[$index - 1];
    }
    return $self;
}

my %MT_HAS_ATTACHMENT = ( mixed => 1 );

sub has_attachments {
    my ($self) = @_;
    my $mt = $self->multipart;
    return $mt && $MT_HAS_ATTACHMENT{$mt} ? 1 : 0;
}

sub _parse_body {
    my ($self, $struct) = @_;

    if (ref($struct->[0]) eq 'ARRAY') {
        my @tmp = @$struct;
        my $multipart = pop @tmp;
        my $part_id = $self->{part_id} || '';
        $part_id .= '.'
          if $part_id;
        my $i = 0;
        @tmp = map { __PACKAGE__->new({ BODY => $_}, $part_id . ++$i) } @tmp;
        $self->{multipart_type} = $multipart;
        $self->{parts} = \@tmp;
    } else {
        $self->{type} = lc $struct->[0];
        $self->{subtype} = lc $struct->[1];
        if ($struct->[2]) {
            my %tmp = @{$struct->[2]};
            $self->{parameters} = \%tmp;
        }
        $self->{cid} = $struct->[3];
        $self->{description} = $struct->[4];
        $self->{transfer_encoding} = $struct->[5];
        $self->{encoded_size} = $struct->[6];

        if ($self->content_type eq 'message/rfc822') {
            # continue parsing attached message
            $self->_parse_envelope($struct->[7]);
            $self->_parse_body($struct->[8]);
        }
    }
}

sub _parse_bodystructure {
    my ($self, $struct) = @_;

    if (ref($struct->[0]) eq 'ARRAY') {
        my @tmp = @$struct;

        pop @tmp; pop @tmp; pop @tmp; # XXX: we don't care about
                                      # extension data of multipart
                                      # body yet; does it actually
                                      # contain anything interesting?

        my $multipart = pop @tmp;
        my $part_id = $self->{part_id} || '';
        $part_id .= '.'
          if $part_id;
        my $i = 0;
        @tmp = map { __PACKAGE__->new({ BODYSTRUCTURE => $_}, $part_id . ++$i) } @tmp;
        $self->{multipart_type} = $multipart;
        $self->{parts} = \@tmp;
    } else {
        $self->{type} = lc $struct->[0];
        $self->{subtype} = lc $struct->[1];
        if ($struct->[2]) {
            my %tmp = @{$struct->[2]};
            $self->{parameters} = \%tmp;
        }
        $self->{cid} = $struct->[3];
        $self->{description} = $struct->[4];
        $self->{transfer_encoding} = $struct->[5];
        $self->{encoded_size} = $struct->[6];

        if ($self->content_type eq 'message/rfc822') {
            # continue parsing attached message
            $self->_parse_envelope($struct->[7]);
            $self->_parse_bodystructure($struct->[8]);
        } elsif ($self->type ne 'text') {
            $self->{md5} = $struct->[7];
            if ($struct->[8]) {
                my %tmp = @{$struct->[8]};
                foreach (values %tmp) {
                    if (ref($_) eq 'ARRAY') {
                        my %foo = @{$_};
                        $_ = \%foo;
                    }
                }
                $self->{disposition} = \%tmp;
            }
            $self->{language} = $struct->[9];
        }
    }
}

sub _parse_envelope {
    my ($self, $struct)  = @_;
    $self->{date}        = $struct->[0];
    $self->{subject}     = $struct->[1];
    $self->{from}        = _parse_address($struct->[2]);
    $self->{sender}      = _parse_address($struct->[3]);
    $self->{reply_to}    = _parse_address($struct->[4]);
    $self->{to}          = _parse_address($struct->[5]);
    $self->{cc}          = _parse_address($struct->[6]);
    $self->{bcc}         = _parse_address($struct->[7]);
    $self->{in_reply_to} = $struct->[8];
    $self->{message_id}  = $struct->[9];
}

sub _parse_address {
    my ($adr) = @_;
    if ($adr) {
        $adr = [ map { Net::IMAP::Client::MsgAddress->new($_) } @$adr ];
    }
    return $adr;
}

1;

__END__

=pod

=head1 NAME

Net::IMAP::Client::MsgSummary - parse message (+ subparts) summary info

=head1 SYNOPSIS

This object is created internally in Net::IMAP::Client->get_summaries.
You shouldn't need to instantiate it directly.  You can skip the
SYNOPSIS, these notes are intended for developers.

    my $imap = Net::IMAP::Client->new( ... )
    $imap->select('INBOX');

    # retrieve FETCH lines
    my ($ok, $lines) = $imap->_tell_imap(FETCH => "$msg_id FULL");
    die 'FETCH failed: ' . $imap->last_error
      unless $ok;

    # build parsed tokens
    my @tokens = map { Net::IMAP::Client::_parse_tokens($_) } @$lines;

    # they look like this:
    [ '*', 'MSGID', 'FETCH',
      [ 'FLAGS', [ '\\Seen', '\\Answered' ],
        'INTERNALDATE', '13-Aug-2008 14:43:50 +0300',
        'RFC822.SIZE', '867',
        'ENVELOPE', [
           ...
        ]
      ...
    ]

Basically it's the IMAP response parsed into a Perl structure (array
of tokens).  FIXME: this stuff should be documented in
Net::IMAP::Client.

    # make summaries
    my @summaries = map {
        my $tokens = $_->[3];
        my %hash = @$tokens;
        Net::IMAP::Client::MsgSummary->new(\%hash);
    } @tokens;

    my $summary = shift @summaries;

    print Encode::decode('MIME-Header', $summary->subject);
    print Encode::decode('MIME-Header', $summary->from->[0]);

=head1 DESCRIPTION

This object can represent a message or a message part.  For example,
for a message containing attachments you will be able to call parts()
in order to fetch parsed Net::IMAP::Client::MsgSummary objects for
each part.  Each part in turn may contain other subparts!  For
example, if a part is of type C<message/rfc822> then its C<parts>
method will return it's subparts, if any.

There's a distinction between a message and a message part, although
we use the same object to represent both.  A message will have
additional information, fetched from its ENVELOPE (i.e. C<subject>,
C<from>, C<to>, C<date>, etc.).  For a part only, this information
will be missing.

If all this sounds confusing, you might want to use Data::Dumper to
inspect the structure of a complex message.  See also the
documentation of L<Net::IMAP::Client>'s get_summaries method for an
example.

=head1 API REFERENCE

It contains only accessors that return data as retrieved by the FETCH
command (i.e. you need to undecode it).

=head2 C<new>  # constructor

Parses/creates a new object from the given FETCH data.

=over

=item C<type>

Returns the base MIME type (i.e. 'text')

=item C<subtype>

Returns the subtype (i.e. 'plain')

=item C<parameters>

Returns any parameters passed in BODY(STRUCTURE).  You shouldn't need
this.

=item C<cid>

Returns the part's unique identifier (CID).

=item C<description>

Returns the part's description (usually I<undef>).

=item C<transfer_encoding>

Returns the part's content transfer encoding.  You'll need this in
order to decode binary parts.

=item C<encoded_size>

Returns the size of the encoded part.  This is actually the size in
octets that will be downloaded from the IMAP server if you fetch this
part only.

=item C<content_type>

Shortcut for C<$self->type . '/' .$self->subtype>.

=item C<charset>

Returns the charset declaration for this part.

=item C<filename>

Returns the file name of this part, if found in FETCH response.

=item C<multipart>

Returns the multipart type (i.e. 'mixed', 'alternative')

=item C<parts>

Returns the subparts of this part.

=item C<part_id>

Returns the "id" (path) of this part starting from the toplevel
message, i.e. "2.1" (meaning that this is the first subpart of the
second subpart of the toplevel message).

=item C<md5>

Returns a MD5 of this part or I<undef> if not present.

=item C<disposition>

Returns the disposition of this part (I<undef> if not present).  It's
a hash actually that looks like this:

  { inline => { filename => 'foobar.png' } }

=item C<language>

Returns the language of this part or I<undef> if not present.

=item C<rfc822_size>

Returns the size of the full message body.

=item C<internaldate>

Returns the INTERNALDATE of this message.

=item C<flags>

Returns the flags of this message.

=item C<uid>

Returns the UID of this message.

=item C<date>

Returns the date of this message (from the Date header).

=item C<subject>

Returns the subject of this message.

=item C<from>, C<sender>, C<reply_to>, C<to>, C<cc>, C<bcc>

Returns an array of Net::IMAP::Client::MsgAddress objects containing
the respective addresses.  Note that sometimes this array can be
empty!

=item C<in_reply_to>

Returns the ID of the "parent" message (to which this one has been
replied).  This is NOT the "UID" of the message!

=item C<message_id>

Returns the ID of this message (from the Message-ID header).

=item C<get_subpart> ($path)

Returns the subpart of this message identified by $path, which is in
form '1.2' etc.  Returns undef if no such path was found.

Here's a possible message structure:

 - Container (multipart/mixed) has no path ID; it's the toplevel
   message.  It contains the following subparts:

   1 multipart/related
     1.1 text/html
     1.2 image/png (embedded in HTML)

   2 message/rfc822 (decoded type is actually multipart/related)
     2.1 text/html
     2.2 image/png (also embedded)

C<get_subpart> called on the container will return the respective
Net::IMAP::Client::MsgSummary part, i.e. get_subpart('2.1') will
return the text/html part of the attached message.

=item C<has_attachments>

Tries to determine if this message has attachments.  For now this
checks if the multipart type is 'mixed', which isn't really accurate.

=back

=head1 TODO

Fix C<has_attachments>

=head1 SEE ALSO

L<Net::IMAP::Client>, L<Net::IMAP::Client::MsgAddress>

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
