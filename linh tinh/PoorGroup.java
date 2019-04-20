package com.examplefoobar.utils;

import javax.annotation.concurrent.ThreadSafe;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;

/**
 * A thread-safe container that stores a group ID and members.
 *
 * It can be added <tt>Member</tt> and return a member list as String.
 * Also, it can start and stop a background task that writes a member list to specified files.
 *
 * This class is called a lot, so we need improve it.
 */

@ThreadSafe
public class PoorGroup
{
    // change access level from package to private
    private String groupId;
    private HashSet<Member> members;
    private boolean shouldStop;

    private static final int MULTIPLICATION_FACTOR = 10;
    class Member
    {
        // change access level from package to private
        private String memberId;
        private int age;

        Member(String memberId, int age)
        {
            this.memberId = memberId;
            this.age = age;
        }

        public String getMemberId()
        {
            return memberId;
        }

        public int getAge()
        {
            return age;
        }

//        public boolean equals(Object o)
//        {
//            // If `memberId` matches the other's one, they should be treated as the same `Member` objects.
//            Member member = (Member) o;
//            return this.memberId == member.memberId;
//        }


        @Override
        public boolean equals(Object o) {
            if (this == o)
                return true;
            if ( !(o instanceof Member) )
                return false;

            Member member = (Member) o;

            return this.memberId == member.memberId;
        }

        @Override
        public int hashCode() {
            return memberId != null ? memberId.hashCode() : 0;
        }
    }

    public PoorGroup(String groupId)
    {
        this.groupId = groupId;
        this.members = new HashSet<>();
    }

    public void addMember(Member member)
    {
        members.add(member);
    }

    public String getMembersAsStringWith10xAge()
    {
        StringBuilder membersStringBuilder = new StringBuilder();

        for (Member member : members)
        {
            int ageMultiplied = member.getAge() * MULTIPLICATION_FACTOR;
            String memberId = member.getMemberId();

            // Don't ask the reason why `age` should be multiplied ;)
            membersStringBuilder.append("memberId=" + memberId + ", age=" + ageMultiplied + "\n");
        }
        return membersStringBuilder.toString();
    }

    /**
     * Run a background task that writes a member list to specified files 10 times in background thread
     * so that it doesn't block the caller's thread.
     */

    public void my_startLoggingMemberList10Times(final String outputFilePrimary, final String outputFileSecondary){
        new Thread(new Runnable() {
            @Override
            public void run()
            {
                int i = 0;
                while (!shouldStop)
                {
                    if (i++ >= 10)
                        break;

                    // use try-resources
                    try (FileWriter writerPrimary = new FileWriter(outputFilePrimary);
                         BufferedWriter bufferPrimary = new BufferedWriter(writerPrimary);

                         FileWriter writerSecondary = new FileWriter(outputFileSecondary);
                         BufferedWriter bufferSecondary = new BufferedWriter(writerSecondary) ){

                        bufferPrimary.write( PoorGroup.this.getMembersAsStringWith10xAge() );
                        bufferSecondary.write( PoorGroup.this.getMembersAsStringWith10xAge() );

                    } catch (IOException e){
                        System.out.println("IOEx");
                    }

                    ///////////////
                    FileWriter writerPrimary   = null;
                    FileWriter writerSecondary = null;

                    try {
                        writerPrimary = new FileWriter(new File(outputFilePrimary));
                        writerPrimary.append(PoorGroup.this.getMembersAsStringWith10xAge());

                        writerSecondary = new FileWriter(new File(outputFileSecondary));
                        writerSecondary.append(PoorGroup.this.getMembersAsStringWith10xAge());
                    }
                    catch (Exception e) {
                        throw new RuntimeException("Unexpected error occurred. Please check these file names. outputFilePrimary="
                                + outputFilePrimary + ", outputFileSecondary=" + outputFileSecondary);
                    }
                    finally {
                        try {
                            if (writerPrimary != null)
                                writerPrimary.close();

                            if (writerSecondary != null)
                                writerSecondary.close();
                        }
                        catch (Exception e) {
                            // Do nothing since there isn't anything we can do here, right?
                            System.out.println("Close file exception, error: " + e.getMessage());
                        }
                    }

                    try {
                        Thread.sleep(1000);
                    }
                    catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        }).start();
    }

    public void startLoggingMemberList10Times(final String outputFilePrimary, final String outputFileSecondary)
    {
        new Thread(new Runnable() {
            @Override
            public void run()
            {
                int i = 0;
                while (!shouldStop)
                {
                    if (i++ >= 10)
                        break;

                    FileWriter writerPrimary   = null;
                    FileWriter writerSecondary = null;

                    try {
                        writerPrimary = new FileWriter(new File(outputFilePrimary));
                        writerPrimary.append(PoorGroup.this.getMembersAsStringWith10xAge());

                        writerSecondary = new FileWriter(new File(outputFileSecondary));
                        writerSecondary.append(PoorGroup.this.getMembersAsStringWith10xAge());
                    }
                    catch (Exception e) {
                        throw new RuntimeException("Unexpected error occurred. Please check these file names. outputFilePrimary="
                                + outputFilePrimary + ", outputFileSecondary=" + outputFileSecondary);
                    }
                    finally {
                        try {
                            if (writerPrimary != null)
                                writerPrimary.close();

                            if (writerSecondary != null)
                                writerSecondary.close();
                        }
                        catch (Exception e) {
                            // Do nothing since there isn't anything we can do here, right?
                            System.out.println("Close file exception, error: " + e.getMessage());
                        }
                    }

                    try {
                        Thread.sleep(1000);
                    }
                    catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        }).start();
    }

    /**
     * Stop the background task started by <tt>startPrintingMemberList()</tt>
     * Of course, <tt>startLoggingMemberList</tt> can be called again after calling this method.
     */
    public void stopPrintingMemberList()
    {
        shouldStop = true;
    }
}
