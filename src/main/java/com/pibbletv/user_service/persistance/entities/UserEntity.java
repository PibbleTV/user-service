package com.pibbletv.user_service.persistance.entities;

import jakarta.validation.constraints.NotNull;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import jakarta.validation.constraints.NotEmpty;
import org.hibernate.validator.constraints.Length;

import java.util.UUID;

@Table("users")
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserEntity {

    @Id
    private Long id;

    @NotNull
    @NotEmpty
    private UUID userId;

    @NotEmpty(message = "Username is required")
    @Length(min = 3, max = 17, message = "Username must be between 3 and 17 characters")
    @Column("username")
    private String username;

    private byte[] bgImage;

    private byte[] profileImage;

    @Column("is_banned")
    private Boolean isBanned;

}